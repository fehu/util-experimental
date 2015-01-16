package feh.util.exec

import org.specs2._
import scala.concurrent.duration.FiniteDuration
import feh.util._
import scala.concurrent.{Future, Await}
import scala.util.Try
import org.specs2.specification.Fragments
import org.specs2.matcher.MatchResult
import org.specs2.execute.AsResult


trait ProcessWrappersSpecHelper extends ExecUtils{
  implicit def asys = Tests.asys
  def halfSecond = FiniteDuration(500, "millis")
  def oneSecond = FiniteDuration(1, "second")
  def aSecondAndAHalf = FiniteDuration(1500, "millis")

  def execSuccess() = exec("ls")
  def execFailure() = exec("scala", "-no-such-option")

}

class ProcessWrappersSpec extends Specification
  with ProcessWrappersSpecHelper with ProcessWrappers
{
  def is =                                                                    s2""" ${"ProcessWrappers".title}
  `ProcessWrappers` trait provides                                            $end
    `RunningProcess` and an conversion `.wrap` from `Process`                 $br$runningProcess
    `FinishedProcess` container                                               $end
                                                                              """

  def runningProcess =                                                                                  s2"""
 $t$t A `RunningProcess` listens to process exit and creates a `FinishedProcess`, that can be accessed  $end

            Synchronously                                                            $accessFinishedSync$end
            Asynchronously                                                          $accessFinishedAsync$end
                                                                                                        """

  def accessFinishedSync =                                              s2"""
          by method `await`                                             ${sync.await}
                                                                        """

  def accessFinishedAsync =                                             s2"""
          by method `onComplete`                                        ${async.complete}
          by methods `onSuccess`/`onFailure`                            ${async.successAndFail}
                                                                        """

  object async{
    def successAndFail = {
      var r1, r2 = ""

      val s = execSuccess().wrap
      val f = execFailure().wrap

      s.onSuccess(_ => r1 += "success"); s.onFailure(_ => r1 += "failure")
      f.onSuccess(_ => r2 += "success"); f.onFailure(_ => r2 += "failure")

      Await.ready(Future.sequence(Seq(s.asFuture, f.asFuture)), halfSecond)
      r1 === "success" and r2 === "failure"
    }

    def complete = {
      val r1, r2 = new StringBuilder

      val s = execSuccess().wrap
      val f = execFailure().wrap

      def foo(log: StringBuilder): FinishedProcess => Unit = {
        case p if p.success =>  log ++= "success"
        case _ =>               log ++= "failure"
      }

      s.onComplete(foo(r1))
      f.onComplete(foo(r2))

      Await.ready(Future.sequence(Seq(s.asFuture, f.asFuture)), halfSecond)
      r1.mkString === "success" and r2.mkString === "failure"
    }
  }

  object sync{
    def await = {
      val s = execSuccess().wrap
      val f = execFailure().wrap

      val e1 = s.await(halfSecond)
      val e2 = f.await(halfSecond)

      (e1.exitCode === 0) and (e2.exitCode !== 0)
    }
  }

}

/** runs only on unix
*/
class ProcessReaderWrappersSpec extends Specification with ProcessReaderWrappers
  with ProcessWrappersSpecHelper with UnixExecUtils
{
  def is = "ProcessReaderWrappers".title ^ (
    if (Platform.check.isUnix) theSpec
    else skipped("Only unix platforms are supported for this test, currently using " + Platform.name): Fragments
  )

  override protected def ensureUnix = false

  def theSpec =                                                                                       s2"""
`ProcessReaderWrappers` trait                                                                         $end
 reimplements `ExecUtils`'s execution methods the way the process input streams would be available $execRedirects$end
  provides wrapping to `RunningProcessReader`, that                                                $br$testRunning
                                                                                                      """
  def testRunning =                                                                                   s2"""
      reads process streams asynchronously                                                            $end
            output                                                                        $readsOutput$end
            error                                                                          $readsError$end
      may be represented as `Future`                                                    $readsAsFuture$end
      upon process completion constructs `FinishedProcessWithReadStreams`, that                       $br$testFinished
                                                                                                      """
  def testFinished =                                                                                  s2"""
        is accessed from the corresponding `RunningProcessReader`                                     $end
          synchronously (.await)                                                                      $end
            for successful execution (exit code = 0)                                                  $finSyncSucc
            for unsuccessful execution (exit code != 0)                                               $finSyncErr
          asynchronously (onCompleteReading/onSuccessReading/onFailureReading)                        $end
            for successful execution (exit code = 0)                                                  $finAsyncSucc
            for unsuccessful execution (exit code != 0)                                               $finAsyncErr
        contains full streams output                                                                  $end
          output                                                                                      ${isFull().output}
          error                                                                                       ${isFull().error}
                                                                                                      """

  def execRedirects = {
    val p = exec("ls")
    val is = p.getInputStream
    val ps = p.getErrorStream

    (is mustNotEqual null) and (is.read mustNotEqual -1) and (ps mustNotEqual null)
  }

  def readsOutput = reads("running iter ", error = false)

  def readsError = reads("error in iter ", error = true)

  def readsAsFuture = {
    val p = echoEveryProcess("test", echo = Scripts.EchoEvery.Normal, errorInFin = false)
    val fut = p.asFuture

    Try{ Await.ready(fut, aSecondAndAHalf) } must beSuccessfulTry
  }

  def testSuccessOutputAndError(read: List[String]) = read.length === 6 and read.last === "Done"
  def testFailureOutput(read: List[String]) = read.length === 6 and read.last === "Failed"
  def testFailureError(err: List[String]) = {
    val errMsg = "division by 0"
    val rrev = err.reverse
    err.length === 7 and rrev.drop(1).head === "Failed" and (rrev.head must contain(errMsg))
  }

  def finSyncSucc = finSync("running ", "Done", true, aSecondAndAHalf, testSuccessOutputAndError)
  def finSyncErr = finSync("error ", "Failed", false, aSecondAndAHalf, testFailureOutput, testFailureError)
  def finAsyncSucc = finAsync("running ", "Done", true, aSecondAndAHalf, testSuccessOutputAndError)
  def finAsyncErr = finAsync("error ", "Failed", false, aSecondAndAHalf, testFailureOutput, testFailureError)

  case class isFull(){
    val msg = "this output goes both to 'stdout' and 'stderr'"
    val finMsg = "Test Finished"

    protected lazy val fullTextExpectation = (1 to echoEveryProcess_times map { msg + _ }) :+ finMsg
    protected def readFull(selectRead: FinishedProcessWithReadStreams#Read => List[String]) =
      (p: FinishedProcessWithReadStreams) => selectRead(p.read) mustEqual fullTextExpectation
    protected def awaitAndApply[R : AsResult](wait: FiniteDuration, apply: FinishedProcessWithReadStreams => R) =
      Try{ apply(p.await(wait)) } must beSuccessfulTry.which(identity)

    protected lazy val p = echoEveryProcess(msg, Scripts.EchoEvery.Both, errorInFin = false, finished = finMsg)

    def output = awaitAndApply(aSecondAndAHalf, readFull(_.output))
    def error = awaitAndApply(aSecondAndAHalf, readFull(_.error))
  }


  protected def echoEveryProcess_period = FiniteDuration(200, "millis")
  protected def echoEveryProcess_times = 5
  protected def echoEveryProcess(msg: String,
                                 echo: Scripts.EchoEvery.Echo,
                                 errorInFin: Boolean,
                                 finished: String = "Done") =
  {
    sh(Scripts.echoEvery(echoEveryProcess_period, echoEveryProcess_times, msg,
      appendIterToMsg = true,
      echo = echo,
      finishedMessage = Option(finished),
      errorInFin = errorInFin
    )).withReader
  }

  protected def reads(msg: String, error: Boolean) = {
    def echo = if(error) Scripts.EchoEvery.Error else Scripts.EchoEvery.Normal
    val process = echoEveryProcess(msg, echo, errorInFin = false)

    def mustContain(in: String, iter: Int) = in must contain(msg+iter)
    def getRead =
      if(error) process.read.error
      else process.read.output
    def getTheOther =
      if(error) process.read.output
      else process.read.error

    sleep(200)
    val read1 = getRead
    val test1 = mustContain(read1, 1) and mustContain(read1, 2) //and (read1 must not contain(msg+4))

    sleep(300)
    val read2 = getRead
    val test2 = read2 must contain(read1) and mustContain(read2, 3) //and mustContain(read2, 4) //and mustContain(read1, 5)

    val testOther = getTheOther must beEqualTo("")

    test1 and test2 and testOther
  }

  protected def finSync(msg: String,
                        doneMsg: String,
                        successful: Boolean,
                        waitTime: FiniteDuration,
                        testRead: List[String] => MatchResult[Any],
                        testError: List[String] => MatchResult[Any] = null) =
  {
    val p = echoEveryProcess(msg, Scripts.EchoEvery.Both, errorInFin = !successful, finished = doneMsg)

    val terr = Option(testError) getOrElse testRead

    Try { p.await(waitTime) } must beSuccessfulTry.which{
      finished =>
        finished.success mustEqual successful and
          testRead(finished.read.output) and terr(finished.read.error)
    }
  }
  
  protected def finAsync(msg: String,
                         doneMsg: String,
                         successful: Boolean,
                         testWaitTime: FiniteDuration,
                         testRead: List[String] => MatchResult[Any],
                         testError: List[String] => MatchResult[Any] = null) = 
  {
    val testErr = Option(testError) getOrElse testRead
    var out, err = List.empty[String]
    var success = false
    var failure = false
    
    val p = echoEveryProcess(msg, Scripts.EchoEvery.Both, errorInFin = !successful, finished = doneMsg)
    
    p.onCompleteReading{
      finished =>
        out = finished.read.output
        err = finished.read.error
    }
    p.onSuccess(_ => success = true)
    p.onFailure(_ => failure = true)
    
    Try{ Await.ready(p.asFuture, testWaitTime) } must beSuccessfulTry and
      success.mustEqual(successful) and failure.mustEqual(!successful) and
      testRead(out) and testErr(err)
  }

  private def sleep(millis: Long) = Thread.sleep(millis)

}
