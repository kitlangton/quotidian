package quotidian

import quotidian.syntax.*

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

sealed trait Thing
object Thing:
  case class One(name: String, age: Int) extends Thing
  case class Two(name: String)           extends Thing

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

  final case class Simple(name: String, age: Int)

  def deriveProductImpl[A: Type](using Quotes): Expr[ToExpr[A]] =
    import quotes.reflect.*

    def make(expr: Expr[A], quotes: Expr[Quotes]) =
      val caseDef  = deriveProductCaseDef[A](quotes)
      val fallback = CaseDef(Wildcard(), None, '{ throw new Exception("ToExpr pattern match cannot fail") }.asTerm)
      Match(expr.asTerm, List(caseDef, fallback)).asExprOf[Expr[A]]

    '{
      new ToExpr[A]:
        def apply(value: A)(using quotes: Quotes): Expr[A] =
          import quotes.reflect.*
          ${ make('value, 'quotes) }
    }

//
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

  private def deriveSingletonCaseDef[A: Type](quotesExpr: Expr[Quotes])(using Quotes): quotes.reflect.CaseDef =
    import quotes.reflect.*

    // TODO: Certainly there is a better way to do this
    // True Singleton = case object or enum case with no parameter list
    // val isTrueSingleton = Symbol.of[A].companionModule == Symbol.of[A]
    val isTrueSingleton = Symbol.of[A].primaryConstructor.paramSymss.isEmpty

    val exprMatchTerm = '{ $quotesExpr.asInstanceOf[quoted.runtime.QuoteMatching].ExprMatch }.asTerm
    val exprMatchUnapply =
      exprMatchTerm.selectUnique("unapply").appliedToTypes(List(TypeRepr.of[EmptyTuple], TypeRepr.of[EmptyTuple]))

    val constructAExpr =
      if isTrueSingleton then Ref(TypeRepr.of[A].termSymbol).asExprOf[A]
      else Ref(Symbol.of[A].companionModule).selectUnique("apply").appliedToNone.asExprOf[A]

    val patternHoleImplicit =
      Select
        .unique(
          '{ runtime.Expr }.asTerm
            .selectUnique("quote")
            .appliedToTypes(List(TypeRepr.of[Any]))
            .appliedTo(constructAExpr.asTerm),
          "apply"
        )
        .appliedTo(quotesExpr.asTerm)

    CaseDef(
      Unapply(
        exprMatchUnapply,
        List(patternHoleImplicit),
        List('{ EmptyTuple }.asTerm)
      ),
      None,
      '{ Some(${ constructAExpr }) }.asTerm
    )
  end deriveSingletonCaseDef

  def deriveProductCaseDef[A: Type](quotesExpr: Expr[Quotes])(using Quotes): quotes.reflect.CaseDef =
    import quotes.reflect.*

    val fields = TypeTree.of[A].symbol.caseFields

    val (bindSymbols, binds) = fields.map { field =>
      val fieldType  = field.returnType
      val name       = freshName(field.name)
      val bindSymbol = Symbol.newBind(Symbol.spliceOwner, name, Flags.EmptyFlags, fieldType)
      bindSymbol -> Bind(bindSymbol, Wildcard())
    }.unzip

    def makeNestedSplice(field: Symbol, bind: Symbol) =
      field.returnType.asType match
        case '[t] =>
          val toExpr: Expr[ToExpr[t]] =
            Expr.summon[ToExpr[t]].getOrElse(report.errorAndAbort(s"Cannot summon ToExpr[${field.returnType.show}]"))

          def exprApply(quotesExpr: Expr[Quotes]) = '{ Expr }.asTerm
            .selectOverloaded("apply", List(TypeRepr.of[t]), List(Ref(bind)))
            .appliedTo(toExpr.asTerm)
            .appliedTo(quotesExpr.asTerm)

          val contextFun = '{ (q: quoted.Quotes) ?=> ${ exprApply('q).asExprOf[Expr[t]] } }.asTerm

          '{ runtime.Expr }.asTerm
            .selectOverloaded("nestedSplice", List(TypeRepr.of[t]), List(quotesExpr.asTerm))
            .appliedTo(contextFun)

    val nestedSplices = fields.zip(bindSymbols).map(makeNestedSplice)

    val isTrueSingleton = Symbol.of[A].primaryConstructor.paramSymss.isEmpty
    val applied =
      if isTrueSingleton then Term.companionOf[A]
      else Term.companionOf[A].selectUnique("apply").appliedToArgs(nestedSplices)

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
