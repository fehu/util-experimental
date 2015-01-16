package feh.util.sh

import feh.util.sh.exec.{Managed, LibInfo}
import feh.util.ScalaVersion
import com.typesafe.config.ConfigFactory

object Libs {
  object feh{
    def util(version: String): LibInfo = LibInfo("feh.util", "util", version, Managed.scala)
    def shUtil(version: String): LibInfo = LibInfo("feh.util", "shell-utils", version, Managed.scala)

    lazy val util: LibInfo = util(_root_.feh.util.CurrentVersion.ver)
    lazy val shUtil: LibInfo = shUtil(_root_.feh.util.sh.CurrentVersion.ver)
  }

  object scala{
    def library(ver: ScalaVersion) = LibInfo("org.scala-lang", "scala-library", ver.version, Managed.java)
    def libAll(ver: ScalaVersion) = LibInfo("org.scala-lang", "scala-library-all", ver.version, Managed.java)

    def reflect(ver: ScalaVersion) = LibInfo("org.scala-lang", "scala-reflect", ver.version, Managed.java)
    def compiler(ver: ScalaVersion) = LibInfo("org.scala-lang", "scala-compiler", ver.version, Managed.java)
  }

  object akka{
    def currentVersion = ConfigFactory.load().getString("akka.version")
    def actor(ver: String = currentVersion) = LibInfo("com.typesafe.akka", "akka-actor", ver, Managed.scala)
  }

  def specs2(ver: String) = LibInfo("org.specs2", "specs2", ver, Managed.scala)

}
