package quotidian

import scala.quoted.*

object DeriveFromExprSpecMacro:
  inline def testFromExprPerson(inline personExpr: Person): Unit = ${ personFromExpr('personExpr) }

  def personFromExpr(personExpr: Expr[Person])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    personExpr.value match
      case Some(person) =>
        val message = s"FromExpr(person) = $person"
        '{ println(${ Expr(message) }) }
      case None =>
        report.errorAndAbort(s"Could not derive Person from Expr ${personExpr.show}")
