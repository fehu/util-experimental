package feh.util.scutil

import scala.tools.nsc.interpreter.IMain
import scala.reflect.runtime.{ universe => ru }
import scala.tools.nsc.{interpreter, Settings}
import feh.util._
import scala.tools


object IMainRuTreesExecutor{
  def apply(): IMainRuTreesExecutor with IMainProvider = new DefaultIMainRuTreesExecutorImpl
}

trait IMainRuTreesExecutor{
  protected val iMain: IMain
  protected lazy val global = iMain.global
  protected lazy val ruToolbox = tools.reflect.ToolBox(ru.rootMirror).mkToolBox()

  protected def preprocessTrees: Seq[ru.Tree] => Seq[ru.Tree]
  protected def stringify: Seq[ru.Tree] => String

  def exec(str: String): interpreter.IR.type#Result = execute(ruToolbox.parse(str))
  def exec(expr: ru.Expr[_]*): interpreter.IR.type#Result = execute(expr.map(_.tree): _*)
  def execChildren(expr: ru.Expr[_]*) = execute(expr.flatMap(_.tree.children): _*)
  def execute(tr: ru.Tree*) = iMain.interpret(stringify compose preprocessTrees apply tr) // see iMain.Request()

  protected def findDefinedNames(name: String) = iMain.allDefinedNames.filter(_.decoded == name)
  //todo: use prevRequestList
//  protected def lastRequestForName(name: String) = {
//    val reqs = findDefinedNames(name).flatMap(iMain.requestForName)
//    if(reqs.nonEmpty) Some(reqs.maxBy(_.reqId)) else None
//  }
//  protected def eqStringOrIw(symbName: String, testName: String) =
//    symbName == testName || symbName == ("iw$" + testName)
//  protected def treeOf(name: String, filter: global.Symbol => Boolean) =
//    lastRequestForName(name).flatMap(_.trees.find(_.symbol |> {
//      symb =>
//        filter(symb.asInstanceOf[global.Symbol]) && eqStringOrIw(symb.nameString, name)
//    }))

  def valueOf(name: String) = iMain.valueOfTerm(name)
  def symbolOfTerm(name: String) = iMain.symbolOfTerm(name)
  def symbolOfType(name: String) = iMain.symbolOfType(name)
//  def treeOfTerm(name: String) = treeOf(name, _.isTerm)
//  def treeOfType(name: String) = treeOf(name, _.isType)
}

trait IMainProvider {
  protected def iMain: IMain

  def initialized = iMain.isInitializeComplete
  def initIMainSync() = iMain.initializeSynchronous()
  def initIMainAsync(done: => Unit) = iMain.initialize(done)
}

trait DefaultIMain extends IMainProvider{
  protected lazy val iMainSettings = new Settings() $${
    _.usejavacp.value = true
  }
  protected lazy val iMain = new IMain(iMainSettings)
}

abstract class DefaultIMainRuTreesExecutor extends IMainRuTreesExecutor{
  protected def preprocessTrees = identity
  protected lazy val sourceGen = SourceStringGenerator()
  protected def stringify = sourceGen.asString(_, "\n")
}

class DefaultIMainRuTreesExecutorImpl extends DefaultIMainRuTreesExecutor with DefaultIMain

object IMainRuTreesExecutorApp extends App {
  val executor = IMainRuTreesExecutor()
  import executor._
  import ru._

  def start(){
    initIMainSync()
    execChildren(reify{

      def foo(x: Int)(y: Int) = x * y
      val bar = java.util.UUID.randomUUID()

      trait AA{ def x: Double }
      case class A(x: Double) extends AA
      object OA extends A(0)
    })


    exec("val ttt = 888")

    println("bar = " + valueOf("bar"))
    println("OA = " + valueOf("OA"))

    println("AA symb = " + symbolOfType("AA"))
//    println("A tree = " + treeOfType("A"))
    println("ttt value = " + valueOf("ttt"))
  }

  start()
}