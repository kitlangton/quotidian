package quotidian.examples.show

import quotidian.MacroMirror
import quotidian.MacroMirror.{ProductMacroMirror, SumMacroMirror}
import quotidian.syntax.*
import scala.deriving.*

import scala.quoted.*

object ShowMacro:
  inline def derived[A]: Show[A] = ${ showImpl[A] }

  def showImpl[A: Type](using Quotes): Expr[Show[A]] =
    import quotes.reflect.*
    val mirror = MacroMirror.summon[A].getOrElse {
      report.throwError(s"${Type.show[A]} must be a case class, enum, or sealed trait.")
    }
    mirror match
      case m: ProductMacroMirror[?, ?] => showProductImpl[A](m)
      case m: SumMacroMirror[?, ?]     => showSumImpl[A](m)

  def showSumImpl[A: Type](using Quotes)(mirror: SumMacroMirror[quotes.type, A]): Expr[Show[A]] =
    import quotes.reflect.*

    '{
      new Show[A]:
        def show(a: A): String =
          val sumMirror = ${ Expr.summon[Mirror.SumOf[A]].get }
          val ord       = sumMirror.ordinal(a)
          val instances = ${
            Expr.ofArray(mirror.elemTypes.map { case '[t] =>
              Expr.summon[Show[t]].getOrElse(showImpl[t])
            }*)
          }
          instances(ord).asInstanceOf[Show[Any]].show(a)
    }

  def showProductImpl[A: Type](using Quotes)(mirror: ProductMacroMirror[quotes.type, A]): Expr[Show[A]] =
    import quotes.reflect.*

    def makeString(a: Expr[A]): Expr[String] =
      val labeledValues: List[String | Expr[?]] = mirror.elems.flatMap { elem =>
        import elem.given
        val comma     = if elem == mirror.elems.head then "" else ", "
        val label     = s"$comma${elem.label} = "
        val typeclass = elem.summon[Show]
        val applyShow = '{ $typeclass.show(${ elem.dereference(a) }) }
        List(label, applyShow)
      }
      Expr.interpolatedString((s"${mirror.label}(" :: labeledValues) :+ ")"*)

    '{
      new Show[A]:
        def show(a: A): String =
          ${ makeString('a) }
    }
