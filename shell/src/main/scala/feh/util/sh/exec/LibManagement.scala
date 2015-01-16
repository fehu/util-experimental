package feh.util.sh.exec

import feh.util.{Path, ScalaVersion, ScopedState}
import scala.util.Try
import java.io.File

sealed trait DependencyInfo
  case class LibInfo(group: String, name: String, version: String, meta: LibInfoMeta) extends DependencyInfo{
    def last = copy(version = "_")

    def isScala = PartialFunction.cond(meta){ case Managed(b) => b }

    override def toString = s"$group ${if(isScala) "%%" else "%"} $name % $version"
  }
  case class Import(path: Path, wildcard: Boolean, libs: Seq[LibInfo]) extends DependencyInfo
  {
    def transform(fpath: Path => Path = identity,
                  wildcard: Boolean = this.wildcard,
                  flibs: Seq[LibInfo] => Seq[LibInfo] = identity
                   ) =
      Import(fpath(path), wildcard, flibs(libs))
  }

object DependencyInfo{
  implicit class ImportInfoSeqOps(ii: Seq[DependencyInfo]){
    def extractLibs = ii.flatMap{
      case lib: LibInfo => Seq(lib)
      case Import(_,_,libs) => libs
    }
  }
}

trait LibInfoMeta
  case class Managed(dependsOnScala: Boolean) extends LibInfoMeta
  object Managed{
    def scala = Managed(dependsOnScala = true)
    def java = Managed(dependsOnScala = false)
  }
  case class Unmanaged(path: Path) extends LibInfoMeta
case class LibPath(inf: LibInfo, path: Path)

case class ClassPath(libs: LibPath*){
  def paths = libs.map(_.path)
  def asString = paths.mkString(File.pathSeparator)
  override def toString = s"ClassPath($asString)"
}

trait ClassPathResolver{
  def resolvePath(inf: LibInfo): LibPath
  def searchIvy(inf: LibInfo): Option[Path]
  def searchMaven(inf: LibInfo): Option[Path]

  def classpath(libs: LibInfo*): ClassPath = ClassPath(libs.map(resolvePath): _*)
}

/** Finds scala jars of the version given, an in case of success builds shell commands for `scala` and `scalac`
 *  One cannot rely on [[scala.util.Properties.scalaCmd]] and [[scala.util.Properties.scalacCmd]],
 *    it's not guaranteed that the commands exist in PATH.
 *  Also no [[scala.util.Properties.scalaHome]] would be defined if SCALA_HOME environment variable is not set.
 */
trait ScalaResolver{
  def scala(ver: ScalaVersion): Try[List[String]]
  def scalac(ver: ScalaVersion): Try[List[String]]
}

object ClassPathResolver extends ScopedState[ClassPathResolver](new impl.ClassPathResolverImpl)
object ScalaResolver extends ScopedState[ScalaResolver](new impl.ScalaResolverImpl(ClassPathResolver.get))