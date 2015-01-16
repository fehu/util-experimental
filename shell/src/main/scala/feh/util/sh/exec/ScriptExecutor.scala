package feh.util.sh.exec

import feh.util.file._
import feh.util.sh.UniversalProcessorLibs
import feh.util.{Path, ScalaVersion, Platform, ScopedState}

trait ScriptExecutor {
  def exec(path: Path)
}

trait SourceExecutor {
  def exec(original: Path, source: String)(implicit cp: List[LibPath], ver: ScalaVersion): Process
}


trait ScriptExec extends ScriptExecutor{
  def processor: UniversalProcessorLibs
  def cpResolver: ClassPathResolver
  def executor: SourceExecutor

  lazy val scalaVersion = new ScopedState[ScalaVersion](Platform.scalaVersion)
  
  def exec(path: Path){
    val originalSource = path.file.withInputStream(File.read[String]).recover{
      case err: Throwable => sys.error(s"failed to read file $path: $err")
    }.get

    val source = new StringBuilder(originalSource)
    
    processor.process(source)
    val dependencies = processor.dependenciesProcessors.flatMap(_.withDependencies(source)._2).extractLibs.distinct
    
    implicit val cp = dependencies.map(cpResolver.resolvePath).toList
    implicit val ver = scalaVersion.get

    executor.exec(path, source.mkString)
  }
}
