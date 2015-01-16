package feh.util.sh

import org.specs2.Specification
import feh.util.sh.exec.{ScalaResolver, LibInfo, ClassPathResolver}
import scala.util.{Failure, Success, Try}
import feh.util.{Tests, Platform}
import feh.util.exec.{ProcessReaderWrappers, SbtHelper}
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import org.specs2.execute.{Result, Error}
import org.specs2.matcher.MatchResult
import feh.util.file._

trait ResolversSpecsHelper{
  def scalaVersion = Platform.scalaVersion
}

object ClassPathResolverSpec extends ResolversSpecsHelper{
  def resolver = ClassPathResolver.get
}

class ClassPathResolverSpec extends Specification with SbtHelper{
  def is =                                                  s2""" ${ "ClassPathResolver".title }
    `ClassPathResolver` resolves path to library jar        $end
      in *ivy* by group, name and version                   $end
        cache                                    $inIvyCache$end
        local                                    $inIvyLocal$end
      in *ivy* by group and name (last version)             $end
        cache                                          $todo$end
        local                                          $todo$end
      in *ivy* by name (experimental)                       $end
        cache                                          $todo$end
        local                                          $todo$end
    in local *maven* repo                         $inMaven$end
    unmanaged                                   $unmanaged$end
                                                            """
  import ClassPathResolverSpec._

  implicit def asys = Tests.asys

  def inIvyCache = inIvy(Libs.scala.library(scalaVersion))
  def inIvyLocal: Result =
    Try{ Await.result(projectVersion("util"), FiniteDuration(5, "seconds")) } match{
      case Success(ver) =>
        inIvy(Libs.feh.util(ver)) or { // publish-local the util project and chack again
          Try { Await.result(publishLocal("util"), FiniteDuration(2, "minutes")) } must
            beSuccessfulTry.withValue(true) and
            inIvy(Libs.feh.util(ver))
        }
      case Failure(ex: Exception) => Error("couldn't resolve `util` project version", ex)
    }

  def inMaven = todo
  def unmanaged = todo

  protected def inIvy(lib: LibInfo) = resolver.searchIvy(lib) must beSome.which(_.file.exists())

}

object ScalaResolverSpec extends ResolversSpecsHelper{
  def resolver = ScalaResolver.get
  implicit def asys = Tests.asys
}

class ScalaResolverSpec extends Specification with ProcessReaderWrappers{
  def is =                                              s2""" ${ "ScalaResolver".title }
    `ScalaResolver` searches for given version of       $end
        scala                                           $scala
        scalac                                          $scalac
                                                        """

  import ScalaResolverSpec._

  def scala = resolver.scala(scalaVersion) must beSuccessfulTry.which(getAndConfirmVersion(_, confirmScalaVersion))
  def scalac = resolver.scalac(scalaVersion) must beSuccessfulTry.which(getAndConfirmVersion(_, confirmScalacVersion))



  def confirmScalaVersion(output: String) = output must startWith(expectedScalaVersionStart)
  def confirmScalacVersion(output: String) = output must startWith(expectedScalacVersionStart)

  def expectedScalaVersionStart = s"Scala code runner version ${scalaVersion.version} -- "
  def expectedScalacVersionStart = s"Scala compiler version ${scalaVersion.version} -- "

  protected def getAndConfirmVersion(runArgs: List[String], confirm: String => MatchResult[String]) =
    Try {
      mergingErrorStream{
        exec(runArgs :+ "-version").withReader
          .await(FiniteDuration(2, "seconds")) }
    } must
      beSuccessfulTry
        .which(fin => confirm(fin.read.output.last) )



  //    resolver.scala(scalaVersion) must beSuccessfulTry.which{
  //    scalaArgs => Try {
  //      mergingErrorStream{
  //        exec(scalaArgs :+ "-version").withReader
  //          .await(FiniteDuration(2, "seconds")) }
  //      } must beSuccessfulTry.which(fin =>
  //        confirmScalaVersion(fin.read.output.last)
  //      )
  //
  //  }

}

