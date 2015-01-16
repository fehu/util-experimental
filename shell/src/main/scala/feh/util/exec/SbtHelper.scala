package feh.util.exec

import akka.actor.ActorSystem

trait SbtHelper extends ProcessReaderWrappers{
  lazy val infoPrefixLen = "[info] ".length

  def projectVersion(name: String)(implicit asys: ActorSystem) = sbt(s"project $name", "version").withReader
    .asFuture.map(_.read.output.last.drop(infoPrefixLen))(asys.dispatcher)

  def publishLocal(project: String)(implicit asys: ActorSystem) = execTask(s"project $project", "publish-local")
  def publishLocal(implicit asys: ActorSystem) = execTask("publish-local")

  private def execTask(args: String*)(implicit asys: ActorSystem) =
    sbt(args: _*).withReader
      .asFuture.map(isSuccess _ compose (_.read.output.last))(asys.dispatcher)
  private def isSuccess(lastLine: String) = lastLine.startsWith("[success] ")
}