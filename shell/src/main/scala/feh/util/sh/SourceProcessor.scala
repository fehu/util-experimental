package feh.util.sh

import scala.util.matching.Regex
import feh.util._

/** A SourceProcessor processes different features of the source
 */
trait SourceProcessor {
  /**
   * @param source source to modify
   * @param params depends on implementation
   * @return modified @source
   */
  def process(source: StringBuilder, params: String*): StringBuilder
}

/** A processor that supports shell expressions in script
 */
trait SourceShProcessor extends SourceProcessor with SourceProcessorHelper{
  /** Rewrites shell expression to executable form  
   * @param sh shell expression
   * @return Option(source code running the @sh expression)
   */
  def shExecInj(sh: String): Option[String]
}

/** Processes expressions of form "$key expr" and "$key expr_line1 \ \n expr_line2 ..."
 */
abstract class SourceShLineProcessor(val keyShLine: String) extends SourceShProcessor{

  object shLine{
    def r = keyShLine.r
    def len = keyShLine.length
  }

  def process(source: StringBuilder, params: String*) = source $$ {
    extractAndReplace(source, allMatchesWithAggregatedLines(source.toString(), shLine.r))(shLine.len, 0)(shExecInj): Unit
  }
}

/** Processes expressions of form "$key_start multiline_expressions $key_end"
 */
abstract class SourceShBlockProcessor(val keyShStart: String, val keyShEnd: String) extends SourceShProcessor{

  object shStart{
    def r = keyShStart.r
    def len = keyShStart.length
  }

  object shEnd{
    def r = keyShEnd.r
    def len = keyShEnd.length
  }

  def process(source: StringBuilder, params: String*) = {
    val src = source.toString()
    val starts = shStart.r.findAllMatchIn(src).toList map (_.start)
    val ends = shEnd.r.findAllMatchIn(src).toList map (_.start)
    assert(starts.length == ends.length, "unbalanced sh blocks")
    val ranges = starts.zip(ends).ensuring(_.forall(p => p._1 < p._2), "sh block close precedes open")

    extractAndReplace(source, ranges)(shStart.len, shEnd.len)(shExecInj)
    source
  }

}

case class Replacements(get: List[(Regex, String)])

class SourceRegexReplaceProcessor(replacements: Replacements) extends SourceProcessor{
  def process(source: StringBuilder, params: String*) = replaceByRegex(source)

  def replaceByRegex(code: StringBuilder) = {
    val replaced =
      (code.toString() /: replacements.get){
        case (acc, (rexp, pattern)) => rexp.replaceAllIn(acc, pattern)
      }
    code.clear()
    code ++= replaced
  }

}

object SourceRegexReplaceProcessor{
  lazy val shortVal = """c\$(?<name>\w+)\s*=""".r -> "val ${name} ="
  lazy val shortVar = """\$(?<name>\w+)\s*=""".r -> "var ${name} ="
  lazy val shortArg = """\$(\d+)""".r -> "args($1)"
  lazy val shortObj = "##(?<name>\\w+)".r -> "object ${name}"

  lazy val shortcuts = Replacements(shortVal :: shortVar :: shortArg :: shortObj :: Nil)
}
