package feh.util.sh

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

trait SourceProcessorHelper {
  def aggregateLines(code: String, from: Int): Int ={
    val i = code.indexOf('\n', from)
    if(i == -1) code.length
    else if (code.substring(from, i).trim.lastOption.exists(_ == '\\')) aggregateLines(code, i+1)
    else i

  }

  def allMatchesWithAggregatedLines(code: String, regex: Regex): List[(Int, Int)] =
    regex.findAllMatchIn(code).toList.map{
      m =>
        val end = aggregateLines(code, m.start)
        (m.start, end)
    }

  /** replaces in the @code builder
   * @return (replaced, extracted)
   */
  def extractAndReplace(code: StringBuilder, segments: List[(Int, Int)])
                       (segmentStartLen: Int, segmentEndLen: Int)
                       (replace: String => Option[String]): List[String] = {
    val acc = ListBuffer.empty[String]

    segments.sortBy(_._1).reverse.foreach{
      case (start, end) =>
        val orig = code.substring(start+segmentStartLen, end)
        acc += orig
        replace(orig).foreach( code.replace(start, end+segmentEndLen, _) )
    }
    acc.toList
  }

  def prependToEachLine(what: String, to: String) = to.split('\n').map(what + _).mkString("\n")
  
  def uniteAggregated(separator: String = " ")(str: String) = {
    str.split('\n').map(_.trim) match{
      case Array() => sys.error("empty string")
      case Array(oneLine) => oneLine.trim.ensuring(l => !l.lastOption.exists(_ == '\\'), "unaggregated \\ encountered")
      case ll => {
        val lines = ll.filter("\\" !=).toSeq
        lines.dropRight(1).map(_.ensuring(_.endsWith("\\"), "no aggregation symbol encountered").dropRight(1).trim) :+
          lines.last.ensuring(l => !l.endsWith("\\"), "unexpected \\")
      } mkString separator
    }

  }

  type Replace = String => Option[String]
  object replace{
    def remove: Replace = _ => Some("")
    def comment: Replace = extracted => Some(prependToEachLine("//", extracted))
  }
  
}

object SourceProcessorHelper extends SourceProcessorHelper
