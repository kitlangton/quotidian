package quotidian.examples.eq

import quotidian.*
import quotidian.syntax.*

import scala.deriving.Mirror
import scala.quoted.*

object EqMacros:
  def deriveEq[A: Type](using Quotes): Expr[Eq[A]] =
    import quotes.reflect.*
    MacroMirror.summon[A].getOrElse {
      report.errorAndAbort(s"${Type.show[A]} ${Expr.summon[Mirror.Of[A]]} must be a case class, enum, or sealed trait")
    } match
      case m: MacroMirror.Product[?, ?] => deriveEqProduct[A](m)
      case m: MacroMirror.Sum[?, ?]     => deriveEqSum[A](m)

  private def deriveEqProduct[A: Type](using Quotes)(mirror: MacroMirror.Product[quotes.type, A]): Expr[Eq[A]] =
    import quotes.reflect.*

    def compareElems(x: Expr[A], y: Expr[A]) =
      mirror.elems
        .map { elem =>
          import elem.given
          val eq    = elem.summon[Eq]
          val elemX = elem.get(x)
          val elemY = elem.get(y)
          '{ $eq.eqv($elemX, $elemY) }
        }
        .reduceLeftOption((x, y) => '{ $x && $y })
        .getOrElse(Expr(true))

    '{
      new Eq[A]:
        def eqv(x: A, y: A) = ${ compareElems('x, 'y) }
    }

  private def deriveEqSum[A: Type](using Quotes)(mirror: MacroMirror.Sum[quotes.type, A]): Expr[Eq[A]] =
    import quotes.reflect.*

    '{
      val instances = ${ mirror.deriveArray[Eq]([t] => () => deriveEq[t]) }

      new Eq[A]:
        def eqv(x: A, y: A) =
          val m        = ${ mirror.mirrorExpr }
          val xOrdinal = m.ordinal(x)
          xOrdinal == m.ordinal(y) && instances(xOrdinal).eqv(x, y)
    }
