import feh.util.TestReportsCopy
import sbt._
import Keys._
import Dependencies._

object Build extends sbt.Build {

  val ScalaVersion = "2.11.5"
  val Version = "0.2-SNAPSHOT"

  // // // //  settings presets  // // // //

  val buildSettings = Defaults.coreDefaultSettings ++ Defaults.defaultConfigs ++ Seq (
    organization  := "feh.util",
    scalaVersion  := ScalaVersion,
    scalacOptions in (Compile, doc) ++= Seq("-diagrams"),
    resolvers     += Resolvers.fehu
  )


  lazy val testSettings = TestReportsCopy.settings ++ Seq(
    libraryDependencies += Dependencies.test.specs2,
    TestReportsCopy.copyTestReportsDir <<= baseDirectory(base => Some(base / "test-reports")),
    TestReportsCopy.autoAddReportsToGit := true
  )

  // // // // // //  projects  // // // // // //

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ testSettings ++ Seq(
      publishArtifact := false
    )
  ).aggregate(compiler, shell)

  lazy val compiler = Project(
    id = "scala-compiler-utils",
    base = file("compiler"),
    settings = buildSettings ++ Seq(
      version := Version,
      resolvers += Resolvers.Snapshot.sonatype,
      libraryDependencies <++= scalaVersion {sv =>
        Seq(scala.compiler _, scalaRefactoring _).map(_(sv))
      },
      libraryDependencies += feh.util
    )
  )

  lazy val shell = Project(
    id = "shell-utils",
    base = file("shell"),
    settings = buildSettings ++ testSettings ++ Seq(
      version := Version,
      libraryDependencies ++= Seq(Apache.ioCommons, akka, feh.util)
    )
  )

}
