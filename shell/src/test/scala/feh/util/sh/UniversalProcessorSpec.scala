package feh.util.sh

import org.specs2.Specification
import scala.collection.mutable
import scala.util.Try
import feh.util._
import org.specs2.execute.Result
import feh.util.sh.exec.{Managed, LibInfo}
import org.specs2.matcher.MatchResult
import org.specs2.specification.Text
import scala.collection.immutable.StringLike

object UniversalProcessorSpec{
  def specs2Ver = "2.3.12"

  def uprocessor = UniversalProcessor.get

  def process = uprocessor.process(_: mutable.StringBuilder)
  
  def cKey = uprocessor.configKey
  def aKey = uprocessor.allKey
}

class UniversalProcessorSpec extends Specification{
  def is =                                                                        s2""" ${ "UniversalProcessor".title }
    A `UniversalProcessor` processes source code, reading its configuration from the same source.
    Provides the following features:                                                    $end
        | sh-line |   processes one-line shell expressions                              ${parse().shLine}
        the 'one-line' shell expressions support '\' multi-lining                       ${parse().shLineMulti}
        | sh-block |  processes multi-line shell expression blocks                      ${parse().shBlock}

            | shortcuts | provide the following shortcuts for scala expressions         $end
                var:    $$name = ...      => var name = ...                             ${parse().shortVar}
                val:    c$$name = ...     => val name = ...                             ${parse().shortVal}
                args:   $$1, $$2, ..., $$N  => args(1), args(2), ..., args(N)           ${parse().shortArgs}
                object: ##name           => object name                                 ${parse().shortObject}
            shortcuts must not affect strings and the following expressions             $end
                var:    "$$arg = $$value"                                               ${ignore().shortVar}
                val:    "c$$arg = c$$value"                                             ${ignore().shortVal}
                args:   evidence$$1, evidence$$2                                        ${ignore().shortArg}
                object: x.## max 2, "##ERROR"                                           ${ignore().shortObj}
            Multiline config:                                                           $end
                several ${_conf} keywords in the begining of the source                 ${parse().allHeader}
                several ${_conf} keywords in different parts of the source              ${parse().allAnywhere}
                multi-line, escaped by '\'                                              ${parse().multiline}
        | ${_all} |      key for enabling all the features listed above                 ${parse().allKey}

            Dependency management                                                       $end
                by package *name*, *group* and *version*                                ${depend().byAll}
                    supports scala versioning                                           ${depend().scala.byAll}
                by package *name* and *group*, choosing latest version                  ${depend().autoVer}
                    supports scala versioning                                           ${depend().scala.autoVer}
                by package *name* only                                                  ${depend().byName}
                    supports scala versioning                                           ${depend().scala.byName}

        Quick imports by predefined keys:                                               $end
                'file'       => feh.util.FileUtils._                                    ${imports().file}
                'exec'       => feh.util.ExecUtils._                                    ${imports().exec}
                'akka'       => akka.actor._, akka.pattern.ask, akka.event.Logging,     ${imports().akka}
                                scala.concurrent._, scala.concurrent.duration._         $end

        Predefined imports                                                              ${imports().print(16)}

        Predefined dependencies:                                                        ${depend().print(12)}
                                                                                        """

  import UniversalProcessorSpec._

  def _conf = cKey
  def _all  = aKey

  case class parse(){
    def shLine      = tt(process, 1, ExpectedTransforms.shLine)
    def shLineMulti = tt(process, 1, ExpectedTransforms.shLineMulti)
    def shBlock     = tt(process, 1, ExpectedTransforms.shBlock)
    def shortVar    = short.variable
    def shortVal    = short.const
    def shortArgs   = short.arguments
    def shortObject = short.`object`
    def allHeader   = tt(process, 4, ExpectedTransforms.all)
    def allAnywhere = (testTransform _).tupled(ExpectedTransforms.merged)(process)
    def allKey      = tt(process, 1, ExpectedTransforms.allKey)
    def multiline   = tt(process, 0, ExpectedTransforms.multiline)

    val short = Short(process)
    
    def shSurround(sh: String) = "_root_.feh.util.shell.Exec(\"\"\"" + sh + "\"\"\")"

    def shLineHeader =                 s"$cKey sh-line"
    def shLineExpr =                    "#sh ls -al | grep scala"
    def shLineExpected =   shSurround(  "ls -al | grep scala"  )

    def shBlockHeader =                s"$cKey sh-block"
    def shBlockExpr =                 """#sh>
                                        |   if [ x -ne 0 ]; then res=true
                                        |   else res=false
                                        |   fi
                                        |<sh#""".stripMargin
    def shBlockExpected = shSurround( """
                                        |   if [ x -ne 0 ]; then res=true
                                        |   else res=false
                                        |   fi
                                        |""".stripMargin)

    def shortcutsHeader =              s"$cKey shortcuts"
    def shortcutVal =                   "c$g = 9.80665" -> "val g = 9.80665"
    def shortcutVar =                   "$count   =  0" -> "var count =  0"
    def shortcutArg =                   "$1 +  $2"      -> "args(1) +  args(2)"
    def shortcutObj =                 """
                                        |##settings{
                                        |  var DEBUG = false
                                        |}
                                     |""".stripMargin -> """
                                                           |object settings{
                                                           |  var DEBUG = false
                                                           |}
                                                        |""".stripMargin
    
    def shortcuts = shortcutVal :: shortcutVar :: shortcutArg :: shortcutObj :: Nil
    def shortcutsExpr = shortcuts.map(_._1).mkString("\n")
    def shortcutsExpect = shortcuts.map(_._2).mkString("\n")

    def shLineMultiExpr =             """
                                        |#sh mvn archetype:generate \
                                        |      -DarchetypeGroupId=org.scala-tools.archetypes \
                                        |      -DarchetypeArtifactId=scala-archetype-simple  \
                                        |      -DremoteRepositories=http://scala-tools.org/repo-releases \
                                        |      -DgroupId=org.glassfish.samples \
                                        |      -DartifactId=scala-helloworld \
                                        |      -Dversion=1.0-SNAPSHOT
                                     |""".stripMargin
    def shLineMultiExp = "\n" +
                         shSurround(  """mvn archetype:generate \
                                        |      -DarchetypeGroupId=org.scala-tools.archetypes \
                                        |      -DarchetypeArtifactId=scala-archetype-simple  \
                                        |      -DremoteRepositories=http://scala-tools.org/repo-releases \
                                        |      -DgroupId=org.glassfish.samples \
                                        |      -DartifactId=scala-helloworld \
                                        |      -Dversion=1.0-SNAPSHOT""".stripMargin) +
                                                                  """
                                     |""".stripMargin

    def allConf = s"$cKey $aKey"

    object ExpectedTransforms{
      def shLine      = (shLineHeader,        shLineExpr,         shLineExpected)
      def shBlock     = (shBlockHeader,       shBlockExpr,        shBlockExpected)
      def shortcuts   = (shortcutsHeader,     shortcutsExpr,      shortcutsExpect)
      def shLineMulti = (shLineHeader,        shLineMultiExpr,    shLineMultiExp)

      def allLines = ((eSeq, eSeq, eSeq) /: Seq(shLine, shBlock, shortcuts, shLineMulti)){
        case ((accH, accE, accX), (h, e, x)) => (accH :+ h, accE :+ e, accX :+ x)
      }

      def all = (allLines._1.mkString("\n"), allLines._2.mkString("\n"), allLines._3.mkString("\n"))
      def allKey = (allConf, all._2, all._3)

      def merged = Seq(shLine, shBlock, shortcuts, shLineMulti).map{
        case (h, e, x) => h + "\n" + e -> ("\n" + x)
      }.unzip match{
        case (src, expected) => src.mkString("\n") -> expected.mkString("\n")
      }

      def multiline = (multi(allLines._1), allLines._2.mkString("\n"), allLines._3.mkString("\n", "\n", ""))

      private def multi(lines: Seq[String]) = cKey + lines.map(_.drop(cKey.length)).mkString("\\\n\t\t", "\\\n\t\t", "")
      private def eSeq = Seq.empty[String]
    }

    protected def tt(process: StringBuilder => StringBuilder, prependNewLines: Int, example: (String, String, String)) =
      testTransform(example._1 + "\n" + example._2, "\n"*prependNewLines + example._3)(process)
    protected def getTransform(source: String, process: StringBuilder => StringBuilder) = {
      val src = new StringBuilder(source)
      process(src).mkString
    }
    protected def testTransform(source: String, expected: String)(process: StringBuilder => StringBuilder) = {
      getTransform(source, process) mustEqual expected
    }

    case class Short(process: StringBuilder => StringBuilder) {
      protected lazy val transformQ = mutable.Queue(transform: _*)
      protected var failed = false

      lazy val const        = testTransform(shortcutVal._2)
      lazy val variable     = testTransform(shortcutVar._2)
      lazy val arguments    = testTransform(shortcutArg._2)
      lazy val `object`     = testTransform(shortcutObj._2) and noMore

      protected def testTransform(expected: String): Result =
        if(failed) skipped("skipped due to previous test failure")
        else {
          val l = lines(expected)
          val n = l.length
          takeQ(n) $${
            _.failed.foreach(_ => failed = true)
          } must beSuccessfulTry(l)
        }

      protected def noMore = transformQ must beEmpty
      protected def takeQ(n: Int) = Try{ takeQueue(n) }
      protected def takeQueue(n: Int): Seq[String] =
        if(n == 0) Nil
        else transformQ.dequeue() +: takeQueue(n-1)
      protected def transform = lines(getTransform(shortcutsHeader + "\n" + shortcutsExpr, process))

      private def lines(str: String) = str.split('\n').toList

      // drop the first line, left by conf header
      transformQ.dequeue()
      // init in the correct order
      const; variable; arguments; `object`
    }
  }

  case class ignore(){
    def shortVar = unchanged(shortVarSrc, process)
    def shortVal = unchanged(shortVarSrc, process)
    def shortArg = unchanged(shortVarSrc, process)
    def shortObj = unchanged(shortVarSrc, process)

    def shortVarSrc = """ "$arg = $value" """
    def shortValSrc = """ "c$arg = c$value" """
    def shortArgSrc = "evidence$1; evidence$2"
    def shortObjSrc = "x.## max 2; \"##ERROR\""

    protected def unchanged(src: String, process: StringBuilder => StringBuilder) = new StringBuilder(src) |> {
      sb => process(sb) ==== sb
    }
  }

  def getExtractor[R <: SourceDependencies](collect: PartialFunction[SourceDependencies, R]) =
    uprocessor.dependenciesProcessors.collect(collect).ensuring(_.size == 1).head

  def printInd(what: Seq[Any], indent: Int) = PrintIndents.newBuilder(indent).$${ implicit bp =>
    bp.withDepth(_ => 1) {
      what.foreach(PrintIndents.printlni)
    }
  }.mkString |> (t => Text("\n" + t))

  case class depend(){
    def byAll     = test(key + byAllSrc,    byAllLib,   _ ==== commented(byAllSrc))
    def autoVer   = test(key + autoVerSrc,  autoVerLib, _ ==== commented(autoVerSrc))
    def byName    = test(key + byNameSrc,   byNameLib,  _ ==== commented(byNameSrc))

    lazy val extractor = getExtractor{ case x: SourceDependenciesExtractor => x }

    def key = extractor.dependencyKey

    def byAllSrc = s" org.scala-lang % scala-library-all % ${Platform.scalaVersion.version}"
    def byAllLib = Libs.scala.libAll(Platform.scalaVersion) :: Nil

    def autoVerSrc = " commons-io % commons-io"
    def autoVerLib = LibInfo("commons-io", "commons-io", "_", Managed.java) :: Nil

    def byNameSrc = " %scala-reflect"
    def byNameLib = LibInfo("_", "scala-reflect", "_", Managed.java) :: Nil

    object scala {
      def byAll     = test(key + byAllSrc,    byAllLib,   _ ==== commented(byAllSrc))
      def autoVer   = test(key + autoVerSrc,  autoVerLib, _ ==== commented(autoVerSrc))
      def byName    = test(key + byNameSrc,   byNameLib,  _ ==== commented(byNameSrc))

      def byAllSrc = s" org.specs2 %% specs2 % $specs2Ver"
      def byAllLib = Libs.specs2(specs2Ver) :: Nil

      def autoVerSrc = "feh.util %% util"
      def autoVerLib = Libs.feh.util.last :: Nil

      def byNameSrc = "%% akka-actor"
      def byNameLib = LibInfo("_", "akka-actor", "_", Managed.scala) :: Nil
    }

    def commented = SourceProcessorHelper.replace.comment andThen (_.get)

    def test(src: String, expected: Seq[LibInfo], testSrc: String => MatchResult[String]) = {
      val s = new StringBuilder(src)
      val deps = extractor.withDependencies(s)._2.distinct

      deps mustEqual (extractor.predefinedDependencies ++ expected) and testSrc(s.mkString)
    }

    def print = printInd(extractor.predefinedDependencies, _: Int)
  }

  case class imports() {
    def file = test(key + " file", fileExp, Nil) // lib is auto imported
    def exec = test(key + " exec", execExp, Nil) // lib is auto imported
    def akka = test(key + " akka", akkaExp, Libs.akka.actor() :: Nil)

    lazy val extractor = getExtractor{ case x: SourceImportsExtractor => x }

    def key = extractor.importKey

    def fileExp = "import feh.util.FileUtils._"
    def execExp = "import feh.util.ExecUtils._"
    def akkaExp="""import akka.actor._
                  |import akka.pattern.ask
                  |import akka.event.Logging
                  |import scala.concurrent._
                  |import scala.concurrent.duration._""".stripMargin

    def expectedHead = extractor.toString(extractor.predefinedImports) + "\n"

    def test(expr: String, expected: String, libs: Seq[LibInfo]) = {
      val src = new StringBuilder(expr)
      val deps = extractor.withDependencies(src)._2.extractLibs.distinct

      splitAndSort(src) ==== splitAndSort(expectedHead + expected) and
        deps ==== (extractor.predefinedImports.extractLibs ++ libs)
    }

    private def splitAndSort(str: StringLike[_]) = str.split('\n').sorted

    def print = "\n" + extractor.toString(extractor.predefinedImports, _: Int)
  }
}
