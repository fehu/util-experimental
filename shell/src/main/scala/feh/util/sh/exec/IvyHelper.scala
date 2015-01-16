package feh.util.sh.exec

import feh.util.file._
import feh.util._

trait IvyHelper {
  def ivyRoot = sys.props("user.home") / ".ivy2" inCase (_.file.isDirectory)

  lazy val ivyXmlPattern = """ivy-([\.-_]&&\w&&\d+)\.xml""".r

  def ivyListVersions(dir: Path, inf: LibInfo) = (dir / inf.group / inf.name).file
    .inCase(_.isDir)
    .map{
    _.ls()
      .flatMap(ivyXmlPattern findFirstMatchIn _.name)
      .map(_.group(0))
  }
    .getOrElse(Nil)

  def searchIvy(root: Path, inf: LibInfo): Option[Path] = searchIvyCache(root, inf) orElse searchIvyLocal(root, inf)

  def searchIvyLocal(root: Path, inf: LibInfo) = {
    val name =  packageName(inf)
    (root / "local" / inf.group / name / inf.version / "jars" / s"$name.jar").inCase(_.file.exists())
  }

  def searchIvyCache(root: Path, inf: LibInfo) = {
    val name = packageName(inf)
    (root / "cache" / inf.group / name / "jars" / s"$name-${inf.version}.jar").inCase(_.file.exists())
  }

  protected def packageName(inf: LibInfo) = inf.meta match {
    case Managed(dependsOnScala) =>
      if(dependsOnScala) inf.name + "_" +
        Platform.scalaVersion.version.split('.').take(2).mkString(".")
      else inf.name
    case Unmanaged(_) => sys.error("unmanaged dependencies are not supported by ivy")
  }

}

object IvyHelper extends IvyHelper