package feh.util.sh.exec.impl

import feh.util.sh.exec.ScriptExec
import feh.util.sh.impl.UniversalProcessorImpl

class ScriptExecImpl extends ScriptExec{
  val processor = UniversalProcessorImpl()
  val cpResolver = new ClassPathResolverImpl

  def executor = ???

  def defaultScala = ???
}
