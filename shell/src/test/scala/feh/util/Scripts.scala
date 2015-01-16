package feh.util

import scala.concurrent.duration.FiniteDuration

/** Different shell scripts call arguments for `sh` or `exec`
 */
object Scripts extends ExecUtils.ArgsImplicits{

  object EchoEvery {
    trait Echo{
      def asString: String
      override def toString = asString
    }
    case object Error extends Echo { def asString = "true" }
    case object Normal extends Echo { def asString = "false" }
    case object Both extends Echo { def asString = "both" }
  }

  def echoEvery(period: FiniteDuration,
                times: Int,
                message: String,
                appendIterToMsg: Boolean,
                echo: EchoEvery.Echo,
                finishedMessage: Option[String],
                errorInFin: Boolean) =
  {
    import Boolean.asNum
    implicit def double = Double(4)
    args(getResourcePath("/echoEvery.sh"), period.toMillis / 1000d, times, message, appendIterToMsg, echo.asString,
      errorInFin, finishedMessage.isDefined, finishedMessage.getOrElse(""))
  }

  private def getResourcePath(name: String) = getClass.getResource(name).getFile
}
