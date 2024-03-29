// package quotidian

// import scala.quoted.*

// object MacroMirrorSpecMacro:
//   inline def macroMirrorTest[A]: String             = ${ macroMirrorTestImpl[A] }
//   inline def constructProductTest[A](args: Any*): A = ${ constructProductTestImpl[A]('args) }

//   def macroMirrorTestImpl[A: Type](using Quotes): Expr[String] =
//     import quotes.reflect.*
//     MacroMirror.summon[A] match
//       case Some(singletonMirror: MacroMirror.Singleton[?, ?]) =>
//         Expr(s"MacroMirror: $singletonMirror\n${singletonMirror.expr.show}")
//       case Some(productMirror: MacroMirror.Product[?, ?]) =>
//         Expr(s"MacroMirror: $productMirror")
//       case Some(sumMirror: MacroMirror.Sum[?, ?]) =>
//         Expr(s"MacroMirror: $sumMirror\nordinalOfBanana: ${sumMirror.ordinal[Fruit.Orange]}")
//       case None =>
//         Expr("No MacroMirror found")

//   def constructProductTestImpl[A: Type](args: Expr[Seq[Any]])(using Quotes): Expr[A] =
//     import quotes.reflect.*
//     MacroMirror.summon[A] match
//       case Some(productMirror: MacroMirror.Product[quotes.type, A]) =>
//         val argsExpr = args.asTerm.underlyingArgument.asExprOf[Seq[Any]]
//         val argsList = argsExpr match
//           case Varargs(args) => args
//         val argTerms = argsList.map(_.asTerm)
//         productMirror.construct(argTerms)

//       case Some(otherMirror) =>
//         report.errorAndAbort(s"`constructProductTest` can only be used with product types, but got $otherMirror")

//       case None =>
//         report.errorAndAbort(s"No MacroMirror found")
