package feh.util.scutil

import scala.tools.nsc.interpreter.{Parsed, JLineCompletion, IMain}
import feh.util._

trait CompletionProvider {
  def complete(text: String, position: Int, verbosity: Option[Int]): List[String]
}

class ScalaCompletionProvider(protected val iMain: IMain) extends CompletionProvider with IMainProvider{
  protected val completion = new JLineCompletion(iMain)

  case class YParam(reversedInput: List[Char], result: StringBuilder, previous: Char, state: State)
  case class State(insideStringLiteral: Boolean = false, inWord: Boolean = false, var spaceToDotReplaced:Int = 0)
  case class CurrentFragment(fragment: String, position: Int)

  def getCurrentFragment(text: String, position: Int) ={
    val substrStart = text.substring(0, position)
    val substrEnd = text.substring(position).takeWhile(_.isLetterOrDigit)
//    println(s"substrStart=$substrStart, substrEnd=$substrEnd")
    val substr = substrStart ++ substrEnd
    val posFromEnd = substrEnd.length

    val fr = Y[YParam, String](
      rec => {
        case YParam(h :: t, res, prev, state) if h.isLetterOrDigit =>
//          println(s"YParam: h.isLetterOrDigit: h=$h, res=$res, prev=$prev, state=$state")
          prev match{
            case '.' => res += '.'
            case ' ' if state.spaceToDotReplaced == 0 => 
              state.spaceToDotReplaced += 1
              res += '.'
            case ' ' => res.reverse.mkString
            case _ => 
          }
          if(state.inWord) rec(YParam(t, res += h, h, state))
          else rec(YParam(t, res += h, h, state.copy(inWord = true)))
        case YParam(h :: t, res, prev, state) if !state.insideStringLiteral && (h.isWhitespace || h == '.') =>
//          println(s"YParam: h.isWhitespace || h == '.': h=$h, res=$res, prev=$prev, state=$state")
          if(state.inWord) rec(YParam(t, res, h, state.copy(inWord = false)))
          else h match{
            case '.' if prev == '.' => res.reverse.mkString                                                             //some kind of error - return
            case _ => rec(YParam(t, res, if(prev == '.') prev else h, state))
          }
        case YParam(Nil, res, _, state) =>
//          println(s"YParam: Nil: res=$res, state=$state")
          res.reverse.mkString
        case x@YParam(_, res, _, _) =>
//          println(s"YParam: other: $x")
          res.reverse.mkString
      }
    )(
      YParam(substr.reverse.toList, StringBuilder.newBuilder, '_', State())
    )

    CurrentFragment(fr, fr.length - posFromEnd)
  }


  def complete(text: String, position: Int, verbosity: Option[Int]) = {
    val CurrentFragment(fr, newPosition) = getCurrentFragment(text, position)
//    println(s"fr=$fr, newPosition=$newPosition")
    val parsed = Parsed.dotted(fr, newPosition)
    completion.topLevelFor(verbosity.map(parsed withVerbosity) getOrElse parsed)
  }
}
