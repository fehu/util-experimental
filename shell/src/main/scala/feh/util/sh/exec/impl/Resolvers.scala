package feh.util.sh.exec.impl

import feh.util.sh.exec._
import feh.util._
import feh.util.file._
import feh.util.sh.Libs
import scala.util.Try

class ClassPathResolverImpl extends ClassPathResolver with IvyHelper{
  def resolvePath(inf: LibInfo) = inf.meta match {
    case Unmanaged(path) => LibPath(inf, path).ensuring(path.file.exists(), s"file not found for $inf")
    case Managed(_) =>
      val found = searchIvy(inf) /*orElse searchMaven(inf)*/ getOrElse sys.error(s"$inf not found in ivy")
      LibPath(inf, found)
  }

  protected def ivy = ivyRoot getOrElse sys.error("ivy not found")

  def searchIvy(inf: LibInfo) = searchIvy(ivy, inf)
  def searchMaven(inf: LibInfo): Option[Path] = ??? // todo

}


class ScalaResolverImpl(getClassPathResolver: => ClassPathResolver) extends ScalaResolver with ExecUtils.ArgsImplicits{
  def scala(ver: ScalaVersion) = scalacClasspth(ver).map( javaCall(_, MainGenericRunner) )
  def scalac(ver: ScalaVersion) = scalacClasspth(ver).map( javaCall(_, Main) )

  protected def MainGenericRunner = "scala" / "tools" / "nsc" / "MainGenericRunner"
  protected def Main = "scala" / "tools" / "nsc" / "Main"

  protected def javaCall(cp: ClassPath, main: Path) =
    args("java", "-cp",  cp.asString, "-Dscala.usejavacp=true", main.mkString(".")).toList

  protected def scalaJars(ver: ScalaVersion) = Try {
    Libs.scala.library _ :: Libs.scala.compiler _ :: Libs.scala.reflect _ :: Nil map (_(ver))
  } 
    
  protected def scalacClasspth(ver: ScalaVersion) = scalaJars(ver).map{
    libs => getClassPathResolver.classpath(libs: _*)
  } 
}

/*
class ScalaResolverOldImpl(implicit asys: ActorSystem) extends ScalaResolver with ProcessReaderWrappers{
  val scalaHome = sys.env.getOrElse("SCALA_HOME", sys.error("No SCALA_HOME environment variable defined!"))
  def scalaExecutable = scalaHome / "bin" / "scala"
  def scalacExecutable = scalaHome / "bin" / "scalac"

  lazy val VersionRegex = "Scala .* version (.*) --.*".r

  protected def getVersion(execPath: Path): String = {
    val p = exec(execPath, "-version").withReader.await(5 seconds)
    assert(p.success, s"error calling $execPath -version: " + p.read.error.mkString("\n"))
    p.read.output match {
      case VersionRegex(ver) :: Nil => ver
      case _ => sys.error("unexpected output by scala: " + p.read.output)
    }

  }

  lazy val scalaVersion = getVersion(scalaExecutable)
  lazy val scalacVersion = getVersion(scalacExecutable)

  def scala(ver: ScalaVersion) = if(ver.complies(scalaVersion)) scalaExecutable else sys.error(s"scala $ver not found")
  def scalac(ver: ScalaVersion) = if(ver.complies(scalacVersion)) scalacExecutable else sys.error(s"scalac $ver not found")
}
*/
