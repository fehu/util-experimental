package feh.util.exec


import feh.util.{Path, ExecUtils}
import akka.actor.{PoisonPill, ActorRef, ActorSystem}
import akka.actor.ActorDSL._
import akka.pattern.ask
import feh.util.file._
import scala.concurrent.duration._
import scala.concurrent.{Future, Await}
import java.io.{IOException, InputStream}
import akka.event.Logging

trait ProcessReaderWrappers extends ProcessWrappers with ExecUtils{
  override def exec(args: Seq[String], workingDir: Path = null) = redirectingStreams{ super.exec(args) }

  protected def delayRead: FiniteDuration = 1 milli span

  implicit class ProcessReaderWrapper(p: Process){
      def withReader(implicit asys: ActorSystem) =
        new RunningProcess(p) with RunningProcessReader
          {
            lazy val readDelay = delayRead
          }
    }

  trait RunningProcessReader extends RunningProcess{
    preader =>

    override def finished = Option(_finished.asInstanceOf[FinishedProcessWithReadStreams])

    override def asFuture = super.asFuture.asInstanceOf[Future[FinishedProcessWithReadStreams]]
    def onCompleteReading(f: (FinishedProcessWithReadStreams) => Unit) = super.onComplete(f compose (_.asInstanceOf[FinishedProcessWithReadStreams]))
    def onSuccessReading(f: (FinishedProcessWithReadStreams) => Unit) = super.onSuccess(f compose (_.asInstanceOf[FinishedProcessWithReadStreams]))
    def onFailureReading(f: (FinishedProcessWithReadStreams) => Unit) = super.onFailure(f compose (_.asInstanceOf[FinishedProcessWithReadStreams]))

    override def await(timeout: FiniteDuration) = super.await(timeout).asInstanceOf[FinishedProcessWithReadStreams]

    object read{
      def output = _output.mkString 
      def error = _error.mkString

      protected[ProcessReaderWrappers] val _output = new StringBuilder
      protected[ProcessReaderWrappers] val _error = new StringBuilder

      protected[ProcessReaderWrappers] object finished{
        var output: List[String] = _
        var error: List[String] = _
      }
    }

    def readDelay: FiniteDuration

    protected case object Finish

    protected val readingActor = actor(new Act{
      val output = new StringBuilder
      val error = new StringBuilder

      case object Tick

      val is = process.getInputStream
      val es = process.getErrorStream

      implicit def execc = asys.dispatcher
      def schedule(msg: Any) = asys.scheduler.scheduleOnce(readDelay, self, msg)

      def redirectStreams() = {
        val out = File.read[String](is)
        val err = File.read[String](es)

        this.output ++= out
        read._output ++= out
        this.error ++= err
        read._error ++= err
      }

      var originalFinishSender: Option[ActorRef] = None

      var readOnTick = true

      val log = Logging(this)

      become{
        case Tick if readOnTick =>
          redirectStreams()
          schedule(Tick)
        case Tick =>                                      // do nothing
        case Finish =>
          readOnTick = false
          if (sender != self) originalFinishSender = Some(sender)

          process.getOutputStream.close()                 // close OutputStream first just in case
          redirectStreams()                               // read the last

          if (process.exitValue() != 0) process.destroy() // just in case

          if (!(readAll_?(is) && readAll_?(es))) {
            schedule(Finish)
          }
          else {
            read.finished.output = this.output.split('\n').toList
            read.finished.error = this.error.split('\n').toList
            is.close()
            es.close()
            originalFinishSender.foreach(_ ! Finish)
            schedule(PoisonPill)                          // die after last Tick is processed to avoid dead-letters
          }
      }

      self ! Tick
    })

    protected def readAll_?(is: InputStream) =
      try { is.available() == 0 }
      catch{
        case ex: IOException if ex.getMessage == "Stream closed" => true 
      }

    protected def awaitFinishedPreparation = readDelay*10

    override protected def afterAwait(){
      Await.ready((readingActor ? Finish)(awaitFinishedPreparation), awaitFinishedPreparation)
      super.afterAwait()
    }

    override protected def createFinishedProcess() = new FinishedProcess(process) with FinishedProcessWithReadStreams{
      val read = Read(preader.read.finished.output, preader.read.finished.error)
    }
  }

  trait FinishedProcessWithReadStreams extends FinishedProcess{
    case class Read protected[ProcessReaderWrappers] (output: List[String], error: List[String])
    val read: Read
  }

}

object ProcessReaderWrappers extends ProcessReaderWrappers
