package feh.util.scutil

import scala.tools.nsc.Global
import scala.reflect.api.Universe

trait TreeImporter {
  val global: Global
  def importerFrom[U <: Universe](u: U) = new Importer[U](global.mkImporter(u))

  lazy val runtimeTreeImporter = importerFrom(scala.reflect.runtime.universe)

  class Importer[U <: Universe](importer: global.Importer){
    def importTree(tr: U#Tree) = importer.importTree(tr.asInstanceOf[importer.from.Tree])
  }
}
