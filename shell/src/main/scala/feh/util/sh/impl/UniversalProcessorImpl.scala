package feh.util.sh.impl

import feh.util.sh._
import scala.util.matching.Regex
import feh.util.sh.exec.{LibInfo, Import}
import feh.util.Platform

/** Default source processor implementation
 *
 * @param processorByName
 */
class UniversalProcessorImpl(val processorByName: Map[String, SourceProcessor],
                             val configKey: String,
                             importKeyword: String,
                             importByKey: PartialFunction[String, Seq[Import]],
                             importsPredefined: Seq[Import],
                             dependencyKeyword: String,
                             predefinedDependencies: Seq[LibInfo],
                             val confReplacePolicy: SourceProcessorHelper#Replace)
  extends UniversalProcessorLibs
  with ProcessorConfigSourceParser 
  with ProcessorConfigSourceParserAllKey
{
  val configRegex: Regex = """(?<name>\S+)(?:\:(?<args>.*))?""".r
  def configSeparators = Array(';')
  def configArgsSeparators = Array(',')

  def allKey = "#all"

  lazy val dependenciesProcessors =
    new SourceImportsExtractorImpl(importKeyword, importByKey, importsPredefined, replace.remove) ::
    new SourceDependenciesExtractorImpl(dependencyKeyword, predefinedDependencies, replace.comment) :: Nil

}

object UniversalProcessorImpl{
  trait ShExecutor extends SourceShProcessor{
    def shExecInj(sh: String) = Some("_root_.feh.util.shell.Exec(\"\"\"" + sh + "\"\"\")")
  }

  def configKey = "#conf"

  def keyShLine = "#sh "
  def keyShStart = "#sh>"
  def keyShEnd = "<sh#"

  lazy val processors = Map(
    "sh-line" -> new SourceShLineProcessor(keyShLine) with ShExecutor,
    "sh-block" -> new SourceShBlockProcessor(keyShStart, keyShEnd) with ShExecutor,
    "shortcuts" -> new SourceRegexReplaceProcessor(SourceRegexReplaceProcessor.shortcuts)
  )

  def importKeyword = "#import"

  lazy val importKeys = {
    import SourceImportsExtractorImpl._

    Map(
      "file" -> Seq(fileUtils),
      "exec" -> Seq(execUtils),
      "akka" -> Seq(akka.actor, akka.ask, akka.logging, scala.concurrent, scala.duration)
    )
  }

  def importsPredef = SourceImportsExtractorImpl.utils :: Nil

  def dependencyKeyword = "#lib"

  def dependenciesPredef = Libs.scala.library(scalaVersion) :: Libs.feh.util :: Nil

  def confReplace = SourceProcessorHelper.replace.remove

  def apply(processors: Map[String, SourceProcessor] = processors,
            configKey: String = configKey,
            importKeyword: String = importKeyword,
            importKeys: Map[String, Seq[Import]] = importKeys,
            importsPredef: List[Import] = importsPredef,
            dependencyKeyword: String = dependencyKeyword,
            dependenciesPredef: Seq[LibInfo] = dependenciesPredef,
            confReplace: SourceProcessorHelper#Replace = confReplace) =
    new UniversalProcessorImpl(processors, configKey, importKeyword,
                               importKeys, importsPredef,
                               dependencyKeyword, dependenciesPredef, confReplace)

  def scalaVersion = Platform.scalaVersion
}
