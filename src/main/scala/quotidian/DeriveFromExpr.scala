package quotidian

import scala.quoted.*
import scala.compiletime.*
import quotidian.syntax.*

import scala.deriving.Mirror

object DeriveFromExpr:
  inline def derived[A]: FromExpr[A] = ${ deriveImpl[A] }

  def deriveImpl[A: Type](using Quotes): Expr[FromExpr[A]] =
    import quotes.reflect.*
    def reportError = report.errorAndAbort(
      s"Cannot derive FromExpr for ${TypeRepr.of[A].show}. Only case classes, sealed traits, and enums are supported"
    )
    Expr.summon[Mirror.Of[A]].getOrElse(reportError) match
      case '{ $m: Mirror.ProductOf[A] } => deriveProductImpl[A]
      case '{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } } =>
        val cases = TypeRepr.of[types].tupleToList
        deriveSumImpl[A](cases)
  end deriveImpl

  def deriveProductImpl[A: Type](using Quotes): Expr[FromExpr[A]] =
    import quotes.reflect.*

    def makeMatch(using Quotes)(expr: Expr[Expr[A]], quotesExpr: Expr[Quotes]) =
      import quotes.reflect.*
      val unapply  = deriveProductCaseDef[A](quotesExpr)
      val fallback = CaseDef(Wildcard(), None, '{ None }.asTerm)
      Match(expr.asTerm, List(unapply, fallback)).asExprOf[Option[A]]

    '{
      new FromExpr[A]:
        def unapply(expr: Expr[A])(using quotes: Quotes): Option[A] =
          given Quotes = quotes
          import quotes.reflect.*
          ${ makeMatch('expr, 'quotes) }

    }
  end deriveProductImpl

  def deriveSumImpl[A: Type](using Quotes)(cases: List[quotes.reflect.TypeRepr]): Expr[FromExpr[A]] =
    import quotes.reflect.*

    def makeMatch(using Quotes)(expr: Expr[Expr[A]], quotesExpr: Expr[Quotes]) =
      import quotes.reflect.*
      val caseDefs = cases.map { t =>
        t.asType match
          case '[t] => deriveProductCaseDef[t](quotesExpr)
      }
      val fallback = CaseDef(Wildcard(), None, '{ None }.asTerm)
      Match(expr.asTerm, caseDefs.appended(fallback)).asExprOf[Option[A]]

    '{
      new FromExpr[A]:
        def unapply(expr: Expr[A])(using quotes: Quotes): Option[A] =
          import quotes.reflect.*
          ${ makeMatch('expr, 'quotes) }
    }
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

  private def deriveProductCaseDef[A: Type](using Quotes)(quotesExpr: Expr[Quotes]): quotes.reflect.CaseDef =
    import quotes.reflect.*

    val exprUnapply: Term = '{ Expr }.asTerm.selectUnique("unapply")
    val fields            = TypeRepr.of[A].typeSymbol.caseFields
    val fieldTypes        = fields.map(_.returnType.asType)

    if fields.isEmpty then return deriveSingletonCaseDef[A](quotesExpr)

    val exprMatchTerm = '{ $quotesExpr.asInstanceOf[quoted.runtime.QuoteMatching].ExprMatch }.asTerm

    val fieldExprTypes     = fieldTypes.map { case '[t] => TypeRepr.of[Expr[t]] }
    val fieldExprTupleType = TypeRepr.makeTuple(fieldExprTypes)

    val tupleCompanion = Symbol.classSymbol("scala.Tuple" + fields.length).companionModule

    val patternHoles      = fieldTypes.map { case '[t] => '{ runtime.Patterns.patternHole[t] }.asTerm }
    val companionApply    = Term.companionOf[A].selectUnique("apply")
    val applyPatternHoles = companionApply.appliedToArgs(patternHoles)

    val patternHoleImplicit =
      Select
        .unique(
          '{ runtime.Expr }.asTerm
            .selectUnique("quote")
            .appliedToTypes(List(TypeRepr.of[Any]))
            .appliedTo(applyPatternHoles),
          "apply"
        )
        .appliedTo(quotesExpr.asTerm)

    val (binds, unapplyTerms) = fields.map { field =>
      val fieldType  = field.returnType
      val name       = freshName(field.name)
      val bindSymbol = Symbol.newBind(Symbol.spliceOwner, name, Flags.EmptyFlags, fieldType)

      val fromExpr = fieldType.asType match
        case '[t] =>
          '{ scala.compiletime.summonInline[FromExpr[t]] }.asTerm

      bindSymbol -> Unapply(
        exprUnapply.appliedToType(fieldType),
        List(fromExpr, quotesExpr.asTerm),
        List(Bind(bindSymbol, Wildcard()))
      )
    }.unzip

    val constructAExpr   = companionApply.appliedToArgs(binds.map(Ref(_))).asExprOf[A]
    val tupleAppliedType = Ident(tupleCompanion.termRef).selectUnique("unapply").appliedToTypes(fieldExprTypes)
    val exprMatchUnapply =
      exprMatchTerm.selectUnique("unapply").appliedToTypes(List(TypeRepr.of[EmptyTuple], fieldExprTupleType))

    CaseDef(
      Unapply(
        exprMatchUnapply,
        List(patternHoleImplicit),
        List(Unapply(tupleAppliedType, List.empty, unapplyTerms))
      ),
      None,
      '{ Some(${ constructAExpr }) }.asTerm
    )
  end deriveProductCaseDef

  private var _id: Long = 0
  private def freshName(name: String): String =
    _id += 1
    s"$$${name}_${_id}"
