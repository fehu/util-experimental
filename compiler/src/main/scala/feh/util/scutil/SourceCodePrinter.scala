package feh.util.scutil

import scala.reflect.internal.{Flags, ModifierFlags}
import scala.tools.refactoring.sourcegen._
import scala.tools.refactoring.common.{Tracing, PimpedTrees, CompilerAccess}
import feh.util._
import scala.tools.nsc.symtab._
import scala.tools.refactoring.sourcegen.Requisite._
import scala.Some

trait SourceCodePrinter extends PrettyPrinter with PimpedTrees with CompilerAccess with Tracing
  with Indentations with LayoutHelper with Formatting
{
  import global._

  object sourceCodePrinter extends TreePrinting with PrintingUtils
  with MiscPrinters
  with MethodCallPrinters
  with WhilePrinters
  with PatternMatchingPrinters
  with TypePrinters
  with FunctionPrinters
  with ImportPrinters
  with PackagePrinters
  with TryThrowPrinters
  with ClassModulePrinters
  with IfPrinters
  with ValDefDefPrinters
  with SuperPrinters
  with BlockPrinters
  with LiteralPrinters
  with StdNames
  {
    override def ClassDef(tree: ClassDef,
                          mods: List[ModifierTree],
                          name: Name,
                          tparams: List[Tree],
                          impl: Template)
                         (implicit ctx: PrintingContext): Fragment =
    {
      //mods.annotations map traverse
      val mods_ = mods map (m => m.nameString + " ") mkString ""

      val keyword = tree.mods match{
        case mod if mod.isTrait => "trait " // "trait" is a modifier
        case mod if mod.isCase => "case class "
        case _ => "class "
      }

      val body = impl match {
        case TemplateExtractor(Nil :: Nil, _, _, _, _) if mods exists (_.nameString == "case") =>
          Layout("()") ++ pi(impl)
        case _ =>
          pi(impl)
      }

      Fragment(mods_ + keyword + name) ++ pp(tparams, before = "[", separator = ", ", after = "]") ++ body.ifNotEmpty {
        case body if body.asText.startsWith("{") =>
          Layout(" ") ++ body
        case body =>
          body
      }

    }

    override def AssignOrNamedArg(tree: AssignOrNamedArg, lhs: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment =
      rhsToFragment(rhs, lhs.nameString + " = ")

    def rhsToFragment(rhs: Tree, before: String)(implicit ctx: PrintingContext) = Fragment(rhs match{
      case Literal(Constant(null)) => before + "null"
      case Literal(Constant(const)) =>
        const match{
          case string: String => before + "\"" + string + "\""
          case other => before + other.toString
        }
      case other => Requisite.newline(ctx.ind.incrementDefault.current, ctx.newline).getLayout.asText + before + p(other)
    })

    trait TreeReverseHelper{
      object ApplyReversedFlag extends ThreadLocal[Int]{ set(0) }

      def applyReversed[R](func: => R) = {
        ApplyReversedFlag.set(ApplyReversedFlag.get() + 1)
        val res = func
        ApplyReversedFlag.set(ApplyReversedFlag.get() - 1)
        res
      }

      def applyReversed_? = ApplyReversedFlag.get() != 0
    }

    trait AlreadyReverseHelper{
      object AlreadyReversedFlag extends ThreadLocal[Int]{ set(0) }

      def alreadyReversed[R](func: => R) = {
        AlreadyReversedFlag.set(AlreadyReversedFlag.get() + 1)
        val res = func
        AlreadyReversedFlag.set(AlreadyReversedFlag.get() - 1)
        res
      }

      def alreadyReversed_? = AlreadyReversedFlag.get() != 0
    }

    object ApplyHelper extends TreeReverseHelper with AlreadyReverseHelper{

      def reverseApplication(tree: global.Apply): global.Apply = tree match{
        case Apply(Select(left, name), right) if (name.decode.last == ':') && (right.length == 1) =>
          val newRight = left match{
            case appl@ Apply(_, _) => reverseApplication(appl)
            case tree => tree
          }
          global.Apply(global.Select(right.head, name), newRight :: Nil)

      }

      def isOperator(n: Name) = n.isOperatorName && n != nme.CONSTRUCTOR
    }

    override def Apply(tree: Apply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = {
      import ApplyHelper._
      (fun, args) match {
        // for pretty Symbol definitions
        case (s@Select(New(tpt), nme.CONSTRUCTOR), args_) =>
          val overrideApply = tpt match {
            case x: TypeTree =>
              x.symbol.name.decoded match {
                case "Symbol" =>
                  val symbolName = args_ match {
                    case Literal(Constant(name)) :: Nil => name.asInstanceOf[String]
                  }
                  Some(s"'$symbolName")
                case _ => None
              }
            case _ => None
          }

          overrideApply map (Fragment(_)) getOrElse Apply(tree, fun, args)(ctx)

        // for pretty reversed applied binary operators
        case (s@Select(fnc, name), argz) if (name.decode.last == ':') && (argz.length == 1) && !alreadyReversed_? =>
          if(applyReversed_?) alreadyReversed(Apply(tree, fun, args))
          else applyReversed{
            val newTree = reverseApplication(tree)
            val Apply(newFun, newArgs) = newTree
            Apply(newTree, newFun, newArgs)
          }

        // for dropping parenthesis in reversed applied binary operators
        case (s@Select(selector, op), arg :: Nil) if isOperator(op) =>
          def needsParensAroundArguments(t: Tree) = t match {
            case global.Apply(Select(_, op2), _) =>
              def reversedApplied(n: Name) = n.decode.endsWith(":")
              if(reversedApplied(op2)) isOperator(op2) && precedence(op2) < precedence(op)
              else isOperator(op2) && precedence(op2) <= precedence(op)
            case _ => false
          }

          val select_ = p(selector, after = Requisite.Blank) ++ fun.nameString

          if(needsParensAroundArguments(arg)) {
            select_ ++ p(arg, before = " (", after = ")")
          } else {
            select_ ++ p(arg, before = Requisite.Blank)
          }

        case _ => super.Apply(tree, fun, args)
      }
    }

    object AppliedTypeTreeHelper extends TreeReverseHelper{
      def reverseTypeApplication(tree: global.AppliedTypeTree): global.AppliedTypeTree = tree match{
        case AppliedTypeTree(typ, arg1 :: arg2 :: Nil) =>
          val newArg2 = arg2 match  {
            case appl@ AppliedTypeTree(_, _) => reverseTypeApplication(appl)
            case tree => tree
          }
          global.AppliedTypeTree(typ, newArg2 :: arg1 :: Nil)
      }
    }

    // for binary types
    override def AppliedTypeTree(tree: AppliedTypeTree, tpt: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = {
      import AppliedTypeTreeHelper._

      def isTuple = tpt match{
        case Ident(name) =>
          val n = name.decoded
          n == "Pair" ||
            n == "Triple" ||
            """Tuple\d+""".r.matches(n)
        case _ => false
      }

      def isVarArg = {
        PartialFunction.cond(tpt) {
          case Ident(name) => name.decoded == DecodedTypeName(_.REPEATED_PARAM_CLASS_NAME)
        }
      }

      if(isTuple) pp(args, before = "(", separator = ", ", after = ")")
      else if (isVarArg) pp(args) ++ Fragment("*")
      else if (args.length == 2){
        if(applyReversed_?) {
          val head = p(args(1), after = " ")
          val op = p(tpt)
          val tail = p(args(0), before = " ")

          val frag = head ++ op ++ tail

          if(printingParent_?) Fragment("(") ++ frag ++ ")"
          else frag
        }
        else applyReversed{
          val newTree = reverseTypeApplication(tree)
          val AppliedTypeTree(newTpt, newArgs) = newTree
          AppliedTypeTree(newTree, newTpt, newArgs)
        }
      }
      else super.AppliedTypeTree(tree, tpt, args)
    }

    private lazy val flags = {
      import ModifierFlags._
      val l1 = BYNAMEPARAM :: TRAIT :: FINAL :: IMPLICIT :: PRIVATE :: PROTECTED :: SEALED :: OVERRIDE :: CASE :: ABSTRACT :: PARAM :: Nil map (_.toLong)
      l1 ::: LAZY :: Nil
    }

    def extractMods(mods: Modifiers, filter: Long => Boolean = _ => true): List[ModifierTree] =
      if(mods.flags == 0L) Nil
      else flags flatMap (fl => if(filter(fl) && mods.hasFlag(fl)) Some(fl) else None) map (new ModifierTree(_))


    override def ValDef(tree: ValDef, _mods: List[ModifierTree], name: Name, tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = {
      val mods = extractMods(tree.mods, Flag.PARAM !=)

      //! copied from PrettyPrinter

      def needsKeyword(t: ValDef) =
        !t.mods.hasFlag(Flags.PARAM) &&
          !t.mods.hasFlag(Flags.PARAMACCESSOR) &&
          !t.mods.hasFlag(Flags.CASEACCESSOR) &&
          !t.mods.hasFlag(Flags.SYNTHETIC) &&
          !t.symbol.isSynthetic

      //mods.annotations map traverse
      val mods_ = {
        val existingMods = mods withFilter (_.flag != Flag.BYNAMEPARAM) map (m => m.nameString + " ") mkString "" //! modified
        if(!tree.symbol.isMutable && needsKeyword(tree) && !existingMods.contains("val")) {
          existingMods + "val "
        } else {
          existingMods
        }
      }

      val valName = if(tree.symbol.isThisSym && name.toString == "_") { // this: ... =>
        "this"
      } else name.toString.trim

      //! end: copied from PrettyPrinter

      val beforeType = ": " + (
        if(mods.exists(_.flag == Flag.BYNAMEPARAM)) "=> "
        else ""
        )

      Fragment(mods_ + valName) ++ p(tpt, before = beforeType) ++ p(rhs, before = " = ")
    }


    override def DefDef(tree: DefDef, _mods: List[ModifierTree], name: Name, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
                       (implicit ctx: PrintingContext): Fragment =   {
      val modifiers = extractMods(tree.mods) :+ new ModifierTree(Flags.METHOD)
      //mods.annotations map traverse

      val mods_ = {
        val existingMods = modifiers map (m => m.nameString + " ") mkString ""
        if(tree.mods.hasFlag(Flags.STABLE)&& !existingMods.contains("val")) {
          existingMods + "val "
        } else {
          existingMods
        }
      }

      val tparams_ = {
        // Finalize this fragment so that the anywhere-requisite gets applied here
        // and does not match on ] that might come later (see testNewDefDefWithOriginalContent3
        // and testDefDefWithTypeParams).
        pp(tparams, before = "[", after = anywhere("]"), separator = ", ").toLayout
      }

      // if there's existing layout, the type parameter's layout might already contain "()"
      val params_ = printParameterList(vparamss, tparams_.asText)

      val rhs = if(tree.rhs == EmptyTree && !tree.symbol.isDeferred) {
        Fragment("") // " {"+ ctx.newline + ctx.ind.current +"}"
      } else {
        p(tree.rhs, before = Requisite.allowSurroundingWhitespace("=", " = "))
      }

      val resultType = {
        // The `:` has many places where it can hide, not just the adjoining layout,
        // so we have to check several places: if the parameter list is empty, it could
        // even be part of the tparams.
        val colon = new Requisite {
          def isRequired(l: Layout, r: Layout) = {
            !(l.contains(":") || r.contains(":") || {
              (tparams_.withoutComments + params_.withoutComments).matches(".*:\\s*")
            })
          }
          def getLayout = Layout(": ")
        }
        // Finalize the layout so the `:` won't be searched in the rhs.
        p(tpt, before = colon).toLayout
      }

      Fragment(mods_ + tree.nameString) ++ tparams_ ++ params_ ++ resultType ++ rhs

    }

    override def TypeDef(tree: TypeDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], rhs: Tree)
                        (implicit ctx: PrintingContext): Fragment =
      if (rhs.isEmpty) super.TypeDef(tree, extractMods(tree.mods), name, tparams, rhs)
      else Fragment(tree.keyword + " ") ++ super.TypeDef(tree, extractMods(tree.mods), name, tparams, rhs)

    object PrintingParentFlag extends ThreadLocal[Int]{ set(0) }

    def printingParent[R](func: => R): R = {
      PrintingParentFlag.set(PrintingParentFlag.get + 1)
      val res = func
      PrintingParentFlag.set(PrintingParentFlag.get - 1)
      res
    }

    def printingParent_? = PrintingParentFlag.get() != 0

    // copied from PrettyPrinter, printingParent added
    override def printTemplate(tpl: Template, printExtends: Boolean)(implicit ctx: PrintingContext): Fragment = tpl match {
      case TemplateExtractor(params, earlyBody, parents, self, body) =>

        val sup = if(earlyBody.isEmpty) printingParent {
          parents match {
            case Nil => EmptyFragment
            case parent :: traits =>
              val superclass = {
                if(printExtends)
                  p(parent, before = " extends ")
                else
                  p(parent)
              }
              superclass ++ pp(traits, before = " with ", separator = " with ")
          }
        } else {
          ppi(earlyBody, before = " extends {" ++
            indentedNewline, after = newline ++ "}", separator = indentedNewline) ++
            printingParent(pp(parents, before = " with ", separator = " with "))
        }

        val self_ = (self, body) match {
          case (EmptyTree, body) =>
            ppi(body, before = " {" ++ indentedNewline, separator = indentedNewline, after = newline ++ "}")
          case (self, Nil) =>
            pi(self, before = " {" ++ indentedNewline, after = " =>" ++ newline ++ "}")
          case (self, body) =>
            pi(self, before = " {" ++ indentedNewline, after = " =>") ++
              ppi(body, before = indentedNewline, separator = indentedNewline, after = newline ++ "}")
        }

        def emptyParamListIfEmpty = if(params.flatten.isEmpty) Nil else params
        val params_ = printParameterList(emptyParamListIfEmpty, "") // todo !? !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        params_ ++ sup ++ self_
    }

    override def Typed(tree: Typed, expr: Tree, tpt: Tree)(implicit ctx: PrintingContext): Fragment =
      p(expr) ++ ":" ++ p(tpt)


    val identRemovePrefixes = Seq("scala.", "Predef.")

    override def Ident(tree: Ident, name: Name)(implicit ctx: PrintingContext) = {
      if (tree.symbol.isSynthetic && name.toString.contains("$"))
        Fragment("_")
      // Some identifiers start with an unnecessary <root> ident:
      else if (tree.symbol.nameString == "<root>")
        EmptyFragment
      else if(identRemovePrefixes.exists(tree.name.startsWith) || tree.symbol.name == nme.NO_NAME) Fragment(name.toString)
      else Fragment(tree.symbol.fullName)
    }

  }

  trait StdNames{
    /**
     *  relies on internal StdNames realization
     */
    def fullTpeNames = tpnme.asInstanceOf[reflect.internal.StdNames#TypeNames]

    def DecodedTypeName(sel: reflect.internal.StdNames#TypeNames => reflect.internal.StdNames#TypeNames#NameType): String =
      sel(fullTpeNames).decoded
  }

}

