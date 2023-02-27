package quotidian

import scala.quoted.*

object DeriveFromExprSpecMacro:
  inline def testFromExprPerson(inline personExpr: Person): Unit = ${ personFromExpr('personExpr) }
  inline def testFromExprFruit(inline fruitExpr: Fruit): Unit    = ${ fruitFromExpr('fruitExpr) }
  inline def testFromExprJob(inline jobExpr: Job): Unit          = ${ jobFromExpr('jobExpr) }

  def personFromExpr(personExpr: Expr[Person])(using Quotes): Expr[Unit] =
    fromExprTest[Person](personExpr)

  def fruitFromExpr(fruitExpr: Expr[Fruit])(using Quotes): Expr[Unit] =
    fromExprTest[Fruit](fruitExpr)

  def jobFromExpr(jobExpr: Expr[Job])(using Quotes): Expr[Unit] =
    fromExprTest[Job](jobExpr)

  // abstract the common code
  def fromExprTest[A: Type: FromExpr](expr: Expr[A])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    expr.value match
      case Some(value) =>
        val message = s"FromExpr(${TypeRepr.of[A]}) = $value"
        '{ println(${ Expr(message) }) }
      case None =>
        report.errorAndAbort(s"Could not derive ${TypeRepr.of[A]} from Expr ${expr.show}")
