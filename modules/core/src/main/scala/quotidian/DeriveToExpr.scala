package quotidian

import quotidian.syntax.*

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

object DeriveToExpr:
  inline def derived[A]: ToExpr[A] = ${ deriveImpl[A] }

  def deriveImpl[A: Type](using Quotes): Expr[ToExpr[A]] =
    import quotes.reflect.*
    def error = report.errorAndAbort(
      s"Cannot derive ToExpr for ${TypeRepr.of[A].show}. Only case classes, sealed traits, and enums are supported"
    )
    Expr.summon[Mirror.Of[A]].getOrElse(error) match
      case '{ $m: Mirror.ProductOf[A] } => deriveProductImpl[A]
      case '{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } } =>
        val cases = TypeRepr.of[types].tupleToList
        deriveSumImpl[A](cases)

  def deriveProductImpl[A: Type](using Quotes): Expr[ToExpr[A]] =
    import quotes.reflect.*

    def make(expr: Expr[A], quotesExpr: Expr[Quotes]): Expr[Expr[A]] =
      val caseDef  = deriveProductCaseDef[A](quotesExpr)
      val fallback = CaseDef(Wildcard(), None, '{ throw new Exception("ToExpr pattern match cannot fail") }.asTerm)
      Match(expr.asTerm, List(caseDef, fallback)).asExprOf[Expr[A]]

    '{
      new ToExpr[A]:
        def apply(value: A)(using quotes: Quotes): Expr[A] =
          given Quotes = quotes
          import quotes.reflect.*
          ${ make('value, 'quotes) }
    }
  end deriveProductImpl

  def deriveSumImpl[A: Type](using Quotes)(cases: List[quotes.reflect.TypeRepr]): Expr[ToExpr[A]] =
    import quotes.reflect.*

    def makeMatch(expr: Expr[A], quotes: Expr[Quotes]) =
      val caseDefs = cases.map { t =>
        t.asType match
          case '[t] =>
            deriveProductCaseDef[t](quotes)
      }
      val fallback = CaseDef(Wildcard(), None, '{ throw new Exception("ToExpr pattern match cannot fail") }.asTerm)
      Match(expr.asTerm, caseDefs.appended(fallback)).asExprOf[Expr[A]]

    val ex = '{
      new ToExpr[A]:
        def apply(value: A)(using quotes: Quotes): Expr[A] =
          import quotes.reflect.*
          ${ makeMatch('value, 'quotes) }
    }
//
    ex.asInstanceOf[Expr[ToExpr[A]]]
  end deriveSumImpl

  def deriveProductCaseDef[A: Type](quotesExpr: Expr[Quotes])(using Quotes): quotes.reflect.CaseDef =
    import quotes.reflect.*

    val productMirror = MacroMirror.summonProduct[A]
    val fields        = productMirror.elems

    val (bindSymbols, binds) = fields.map { field =>
      // TODO: Use Symbol.freshName when it is no longer experimental
      val bindSymbol = Symbol.newBind(Symbol.spliceOwner, freshName(field.label), Flags.EmptyFlags, field.typeRepr)
      bindSymbol -> Bind(bindSymbol, Wildcard())
    }.unzip

    def makeNestedSplice(field: MirrorElem[quotes.type, A, ?], bind: Symbol) =
      field.asType match
        case '[t] =>
          val toExpr: Expr[ToExpr[t]] =
            '{ scala.compiletime.summonInline[ToExpr[t]] }

          def exprApply(quotesExpr: Expr[Quotes]) =
            '{ Expr }.asTerm
              .selectOverloaded("apply", List(TypeRepr.of[t]), List(Ref(bind)))
              .appliedTo(toExpr.asTerm)
              .appliedTo(quotesExpr.asTerm)
              .asExprOf[Expr[t]]

          val contextFun = '{ (q: quoted.Quotes) ?=> ${ exprApply('q) } }.asTerm

          '{ runtime.Expr }.asTerm
            .selectOverloaded("nestedSplice", List(TypeRepr.of[t]), List(quotesExpr.asTerm))
            .appliedTo(contextFun)

    val nestedSplices = fields.zip(bindSymbols).map(makeNestedSplice)

    val isTrueSingleton = !TypeRepr.of[A].unapplied.termSymbol.isNoSymbol
    val applied         = productMirror.construct(nestedSplices).asTerm

    val makeQuote =
      Select
        .unique(
          '{ runtime.Expr }.asTerm
            .selectUnique("quote")
            .appliedToTypes(List(TypeRepr.of[A]))
            .appliedTo(applied),
          "apply"
        )
        .appliedTo(quotesExpr.asTerm)

    val unapply =
      if isTrueSingleton then Term.companionOf[A]
      else
        Unapply(
          Term.companionOf[A].selectUnique("unapply"),
          List.empty,
          binds
        )

    CaseDef(
      Typed(unapply.asInstanceOf[Term], TypeTree.of[A]),
      None,
      makeQuote
    )

  private var _id: Long = 0
  private def freshName(name: String): String =
    _id += 1
    s"$$${name}_${_id}"
