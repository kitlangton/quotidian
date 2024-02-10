package quotidian.examples.show

import quotidian.MacroMirror
import quotidian.MacroMirror.{Product, Sum}
import quotidian.syntax.*
import scala.deriving.*

import scala.quoted.*

object ShowMacro:
  def deriveShow[A: Type](using Quotes): Expr[Show[A]] =
    import quotes.reflect.*
    val mirror = MacroMirror.summon[A].getOrElse {
      report.throwError(s"${Type.show[A]} must be a case class, enum, or sealed trait.")
    }
    mirror match
      case m: Product[?, ?] => deriveShowProduct[A](m)
      case m: Sum[?, ?]     => deriveShowSum[A](m)

  def deriveShowProduct[A: Type](using Quotes)(mirror: Product[quotes.type, A]): Expr[Show[A]] =
    import quotes.reflect.*

    def makeString(a: Expr[A]): Expr[String] =
      val labeledValues: List[String | Expr[?]] = mirror.elems.flatMap { elem =>
        import elem.given
        val comma     = if elem == mirror.elems.head then "" else ", "
        val label     = s"$comma${elem.label} = "
        val typeclass = elem.summon[Show]
        val applyShow = '{ $typeclass.show(${ elem.get(a) }) }
        List(label, applyShow)
      }

      if mirror.monoType.isSingleton then Expr(mirror.label)
      else Expr.interpolatedString((s"${mirror.label}(" :: labeledValues) :+ ")"*)

    '{
      new Show[A]:
        def show(a: A): String =
          ${ makeString('a) }
    }

  def deriveShowSum[A: Type](using Quotes)(mirror: Sum[quotes.type, A]): Expr[Show[A]] =
    import quotes.reflect.*

    '{
      val instances = ${ mirror.deriveArray[Show]([t] => () => deriveShow[t]) }

      new Show[A]:
        def show(a: A): String =
          val sumMirror = ${ mirror.mirrorExpr }
          val ord       = sumMirror.ordinal(a)
          instances(ord).asInstanceOf[Show[Any]].show(a)
    }
