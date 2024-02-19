// package quotidian

// import scala.quoted.*

// object DeriveFromExprSpecMacro:
//   inline def testRoundTripPerson(inline personExpr: Person): Person = ${ roundTripPerson('personExpr) }
//   inline def testRoundTripFruit(inline fruitExpr: Fruit): Fruit     = ${ roundTripFruit('fruitExpr) }
//   inline def testRoundTripJob(inline jobExpr: Job): Job             = ${ jobFromExpr('jobExpr) }

//   def roundTripPerson(personExpr: Expr[Person])(using Quotes): Expr[Person] =
//     fromToExprRoundTripTest[Person](personExpr)

//   def roundTripFruit(fruitExpr: Expr[Fruit])(using Quotes): Expr[Fruit] =
//     fromToExprRoundTripTest[Fruit](fruitExpr)

//   def jobFromExpr(jobExpr: Expr[Job])(using Quotes): Expr[Job] =
//     fromToExprRoundTripTest[Job](jobExpr)

//   def fromToExprRoundTripTest[A: Type: FromExpr: ToExpr](expr: Expr[A])(using Quotes): Expr[A] =
//     import quotes.reflect.*
//     expr.value match
//       case Some(value) =>
//         Expr(value)
//       case None =>
//         report.errorAndAbort(s"Could not derive ${TypeRepr.of[A]} from Expr ${expr.show}")
