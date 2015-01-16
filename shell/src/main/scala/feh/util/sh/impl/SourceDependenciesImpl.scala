package feh.util.sh.impl

import feh.util.Path
import feh.util.sh._
import feh.util.sh.exec._
import feh.util.file._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import feh.util.sh.exec.Import
import feh.util.sh.exec.LibInfo

class SourceImportsExtractorImpl(val importKey: String,
                                 val importByKey: PartialFunction[String, Seq[Import]],
                                 val predefinedImports: Seq[Import],
                                 val replacePolicy: SourceProcessorHelper#Replace)
  extends SourceImportsExtractor with SourceProcessorHelper
{
  /** reads and removes import definitions from source */
  protected def extractImports(source: StringBuilder, params: Seq[String]) =
    extractAndReplace(source, allMatchesWithAggregatedLines(source.mkString, importKey.r))(
      importKey.length, 0)(replacePolicy) map uniteAggregated() flatMap extract

  def extract(s: String) = s.split("\\s+").toSeq

  def strings(seq: Seq[Import]) = imports(seq).map{
    case (p, i :: Nil) => "import " + (p / i).path.mkString(".")
    case (p, is) => "import " + p.path.mkString(".") + ".{" + is.mkString(", ") + "}"
  }


  protected def imports(inf: Seq[Import]) = {
    val wild = inf.collect { case Import(path, true, _) => path }
    val imports = inf.collect { case Import(path, false, _) => path } |> Path.group
    imports ++ wild.map(_ -> Seq("_"))
  }

  protected def insert(imports: String, in: StringBuilder) = in.insert(0, imports)
}


object SourceImportsExtractorImpl{
  // override standard one
  protected implicit def strToPath: String => Path = _.pathBy('.')

  lazy val utils = Import("feh" / "util", wildcard = true, Seq(Libs.feh.util))
  lazy val fileUtils = utils.transform(_ / "FileUtils")
  lazy val execUtils = utils.transform(_ / "ExecUtils")

  object akka{
    lazy val actor    = Import("akka.actor",          wildcard = true,  akkaLibs)
    lazy val ask      = Import("akka.pattern.ask",    wildcard = false, akkaLibs)
    lazy val logging  = Import("akka.event.Logging",  wildcard = false, akkaLibs)
  }

  object scala{
    lazy val concurrent = Import("scala.concurrent",  wildcard = true,  Nil)
    lazy val duration =   concurrent.transform(_ / "duration")
  }

  private def akkaLibs = Seq(Libs.akka.actor())
}

/** Reads dependencies from a line (or lines, aggregated by '\') and removes the definitions
 * dependencies may are described in a sbt-like form:
 *      org.scala-lang % scala-library-all % 2.11.1     // for exact version
 *      feh.util %% util == feh.util % util_2.11        // for latest version available (for scala 2.11.x in the example)
 *      %scala-library-all, %% specs2                   // to search locally by package name only
 * @param dependencyKey keyword for dependency statement
 */
class SourceDependenciesExtractorImpl(val dependencyKey: String,
                                      val predefinedDependencies: Seq[LibInfo],
                                      val replacePolicy: SourceProcessorHelper#Replace)
  extends SourceDependenciesExtractor with SourceProcessorHelper
{
  import SourceDependenciesExtractorImpl._

  /** reads and removes import definitions from source */
  protected def extractDependencies(source: StringBuilder, params: Seq[String]) =
    extractAndReplace(source, allMatchesWithAggregatedLines(source.mkString, dependencyKey.r))(
      dependencyKey.length, 0)(replacePolicy) map uniteAggregated() flatMap extract

  protected def extract(depsConf: String): Seq[LibInfo] = {
    val src = new StringBuilder(depsConf)
    def inner(regex: Regex) = {
      val (matches, indS, indE) = matchDeps(regex, src).unzip3
      removeSafely(indS zip indE, src)
      matches
    }
    def buildFullDep(scala: Boolean) =
      (_: List[String]) match{ case gr :: nme :: ver :: Nil => LibInfo(gr, nme, ver, Managed(scala)) }
    def buildGNDep(scala: Boolean) =
      (_: List[String]) match{ case gr :: nme :: Nil => LibInfo(gr, nme, "_", Managed(scala)) }
    def buildNameDep(scala: Boolean) =
      (_: List[String]) match{ case nme :: Nil => LibInfo("_", nme, "_", Managed(scala)) }

    inner(FullDependencyScala).map(buildFullDep(scala = true)) ++
    inner(FullDependency)     .map(buildFullDep(scala = false)) ++
    inner(GroupAndNameScala)  .map(buildGNDep(scala = true)) ++
    inner(GroupAndName)       .map(buildGNDep(scala = false)) ++
    inner(NameOnlyScala)      .map(buildNameDep(scala = true))++
    inner(NameOnly)           .map(buildNameDep(scala = false))
  }


  /** shouldn't modify @source;  
    * @return (subgroups, start and end indexes)
    */
  protected def matchDeps(regex: Regex, source: StringBuilder): Seq[(List[String], Int, Int)] =
    regex.findAllMatchIn(source).toList map {
      case m: Match => (m.subgroups, m.start, m.end)
    }

  /** deletes the substring given by @indexes, starting from the last one
   * @param source source to modify
   */
  protected def removeSafely(indexes: Seq[(Int, Int)], source: StringBuilder) = 
    indexes.sortBy(_._1).reverse foreach (source.delete _).tupled

}

object SourceDependenciesExtractorImpl{
  def identifier = """\s*([\w\d[\.\-_]]+)\s*"""
  def % = """\s*%\s*"""
  def %% = """\s*%%\s*"""

  protected def groupAndName              = identifier + %  + identifier
  protected def groupAndNameScala         = identifier + %% + identifier

  lazy val GroupAndName         = groupAndName.r
  lazy val GroupAndNameScala    = groupAndNameScala.r
  lazy val FullDependency       = (groupAndName      + % + identifier).r
  lazy val FullDependencyScala  = (groupAndNameScala + % + identifier).r
  lazy val NameOnly             = (%  + identifier).r
  lazy val NameOnlyScala        = (%% + identifier).r
}