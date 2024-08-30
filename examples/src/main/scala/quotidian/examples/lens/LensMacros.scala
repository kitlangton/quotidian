package quotidian.examples.lens

import quotidian.*
import quotidian.examples.lens.Lens.make
import quotidian.syntax.*

import scala.annotation.meta.companionClass

import quoted.*

object LensMacros:
  def makeLensImpl[S: Type, A: Type](selectorExpr: Expr[S => A])(using Quotes): Expr[Lens[S, A]] =
    import quotes.reflect.*
    selectorExpr.asTerm.underlyingArgument match
      case Lambda(_, select @ Select(s, a)) =>
        val productMirror = MacroMirror.summonProduct[S]

        val elem = productMirror
          .elemForSymbol(select.symbol)
          .getOrElse(
            report.errorAndAbort(s"Invalid selector ${select.show}, must be a field of ${productMirror.monoType.show}")
          )
          .asElemOf[A]

        '{
          new Lens[S, A]:
            def get(s: S)       = ${ elem.get('s) }
            def set(s: S, a: A) = ${ elem.set('s, 'a) }
        }
      case other => report.errorAndAbort(s"Expected a selector of the form `s => a`, but got: ${other}")

  implicit transparent inline def makeLenses[S]: Any = ${ makeLensesImpl[S] }

  def makeLensesImpl[S: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    val productMirror = MacroMirror.summonProduct[S]

    val lensMap = Expr.ofMap(productMirror.elems.map { elem =>
      import elem.asType
      val selector = '{ (s: S) => ${ elem.get('s) } }
      elem.label -> makeLensImpl(selector)
    })

    val refinedType =
      Refinement.of[LensSelector[S]](
        productMirror.elemsWithTypes.map { case (elem, '[a]) =>
          elem.label -> TypeRepr.of[Lens[S, a]]
        }
      )

    refinedType.asType match
      case '[t] =>
        '{ new LensSelector[S]($lensMap).asInstanceOf[t] }

  def refinedTypeFor[S: Type](using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val productMirror = MacroMirror.summonProduct[S]
    Refinement
      .of[LensSelector[S]](
        productMirror.elemsWithTypes.map { case (elem, '[a]) =>
          elem.label -> TypeRepr.of[Lens[S, a]]
        }
      )

private trait LensesFor[A]:
  type Out
  def lenses: Out

private object LensesFor:
  transparent inline given derived[A]: LensesFor[A] = ${ lensesForImpl[A] }

  private def lensesForImpl[A: Type](using Quotes) =
    import quotes.reflect.*
    val lensesExpr = LensMacros.makeLensesImpl[A]
    lensesExpr.asTerm.tpe.asType match
      case '[t] =>
        '{
          new LensesFor[A]:
            type Out = t
            def lenses: t = $lensesExpr.asInstanceOf[t]
        }

trait Cool[A]

trait DeriveLenses:

  given conversion(using
      cc: CompanionClass[this.type],
      lenses: LensesFor[cc.Type]
  ): Conversion[this.type, lenses.Out] =
    _ => lenses.lenses

trait CompanionClass[A]:
  type Type

object CompanionClass:
  transparent inline given [A]: CompanionClass[A] = ${ companionImpl[A] }

def companionImpl[A: Type](using Quotes) =
  import quotes.reflect.*
  val companionClass = TypeRepr.companionClassOf[A]
  if companionClass.typeSymbol.isNoSymbol then report.errorAndAbort(s"No companion class found for ${Type.show[A]}")

  TypeRepr.companionClassOf[A].asType match
    case '[t] =>
      '{
        new CompanionClass[A]:
          type Type = t
      }
