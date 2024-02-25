package quotidian.examples.lens

import quotidian.*
import quotidian.syntax.*

import quoted.*
import quotidian.examples.lens.Lens.make

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

  transparent inline def makeLenses[S] = ${ makeLensesImpl[S] }

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
