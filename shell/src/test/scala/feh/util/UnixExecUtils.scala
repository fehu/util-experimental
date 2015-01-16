package feh.util

import java.io.File

trait UnixExecUtils extends ExecUtils{
  protected def ensureUnix = true

  if(ensureUnix) Platform.assert(_.isUnix, s"Unix Exec Utils Used on ${Platform.name}")

  def sh(script: File, args: String*): Process = sh(fileToArgs(script, args))
  def sh(args: Seq[String]): Process = exec("sh", args: _*)

  private def fileToArgs(file: File, args: Seq[String]) = file.ensuring(_.exists()).getAbsolutePath +: args
}