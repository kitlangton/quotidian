package quotidian

import scala.quoted.*

object MacroMirrorSpecMacro:
  inline def macroMirrorTest[A]: String               = ${ macroMirrorTestImpl[A] }
  inline def constructProductTest[A](args: Any*): A = ${ constructProductTestImpl[A]('args) }

  def macroMirrorTestImpl[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    MacroMirror.summon[A] match
      case Right(singletonMirror: MacroMirror.SingletonMacroMirror[?, ?]) =>
        Expr(s"MacroMirror: $singletonMirror\n${singletonMirror.expr.show}")
      case Right(productMirror: MacroMirror.ProductMacroMirror[?, ?]) =>
        Expr(s"MacroMirror: $productMirror")
      case Right(sumMirror: MacroMirror.SumMacroMirror[?, ?]) =>
        Expr(s"MacroMirror: $sumMirror\nordinalOfBanana: ${sumMirror.ordinal[Fruit.Orange]}")
      case Left(error) =>
        Expr(error)

  def constructProductTestImpl[A: Type](args: Expr[Seq[Any]])(using Quotes): Expr[A] =
    import quotes.reflect.*
    MacroMirror.summon[A] match
      case Right(productMirror: MacroMirror.ProductMacroMirror[quotes.type, A]) =>
        val argsExpr = args.asTerm.underlyingArgument.asExprOf[Seq[Any]]
        val argsList = argsExpr match
          case Varargs(args) => args
        val argTerms = argsList.map(_.asTerm)
        productMirror.construct(argTerms)
      case Right(otherMirror) =>
        report.errorAndAbort(s"`constructProductTest` can only be used with product types, but got $otherMirror")
      case Left(error) =>
        report.errorAndAbort(error)
