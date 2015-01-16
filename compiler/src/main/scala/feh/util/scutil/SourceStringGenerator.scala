package feh.util.scutil

import scala.reflect.runtime.{universe => ru}
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.util.CompilerProvider
import scala.tools.refactoring.sourcegen.{EmptyFragment, Fragment}
import scala.tools.nsc.io._
import feh.util._

trait SourceStringGenerator {
  def asString(tree: ru.Tree): String
  def asString(tree: Seq[ru.Tree], separator: String): String = tree.map(asString) mkString separator
}

object SourceStringGenerator{
  def apply(): SourceStringGenerator = new ScalaRefactoringSourceStringGenerator()
}

class ScalaRefactoringSourceStringGenerator extends SourceStringGenerator
  with Refactoring with CompilerProvider with SourceCodePrinter with TreeImporter
{
  import global._

  def compilationUnitOfFile(f: AbstractFile) = unitOfFile.get(f)

  override def print(t: Tree, ctx: PrintingContext): Fragment =
    if(t.hasExistingCode)
      reusingPrinter.dispatchToPrinter(t, ctx)
    else if(t != null && !isEmptyTree(t))
      sourceCodePrinter.dispatchToPrinter(t, ctx)
    else
      EmptyFragment

  def asString(tree: ru.Tree) = createText(global.ask(runtimeTreeImporter.importTree(tree).lifted))


}

