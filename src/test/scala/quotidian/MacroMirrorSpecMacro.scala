package quotidian

import scala.quoted.*

object MacroMirrorSpecMacro:
  inline def macroMirrorTest[A]: Unit               = ${ macroMirrorTestImpl[A] }
  inline def constructProductTest[A](args: Any*): A = ${ constructProductTestImpl[A]('args) }

  def macroMirrorTestImpl[A: Type](using Quotes): Expr[A] =
    import quotes.reflect.*
    MacroMirror.summon[A] match
      case singletonMirror: MacroMirror.SingletonMacroMirror[?, ?] =>
        report.errorAndAbort(s"MacroMirror: $singletonMirror\n${singletonMirror.expr.show}")
      case productMirror: MacroMirror.ProductMacroMirror[?, ?] =>
        report.errorAndAbort(s"MacroMirror: $productMirror")
      case sumMirror: MacroMirror.SumMacroMirror[?, ?] =>
        report.errorAndAbort(s"MacroMirror: $sumMirror\nordinalOfBanana: ${sumMirror.ordinal[Fruit.Orange]}")

  def constructProductTestImpl[A: Type](args: Expr[Seq[Any]])(using Quotes): Expr[A] =
    import quotes.reflect.*
    MacroMirror.summon[A] match
      case productMirror: MacroMirror.ProductMacroMirror[quotes.type, A] =>
        val argsExpr = args.asTerm.underlyingArgument.asExprOf[Seq[Any]]
        val argsList = argsExpr match
          case Varargs(args) => args
        val argTerms = argsList.map(_.asTerm)
        productMirror.construct(argTerms)
      case otherMirror =>
        report.errorAndAbort(s"`constructProductTest` can only be used with product types, but got $otherMirror")
