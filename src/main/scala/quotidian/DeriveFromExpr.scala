package quotidian

import scala.quoted.*
import scala.compiletime.*
import quotidian.syntax.*

import scala.deriving.Mirror

object DeriveFromExpr:

  inline def derived[A]: FromExpr[A] = ${ deriveImpl[A] }

  def deriveImpl[A: Type](using Quotes): Expr[FromExpr[A]] =
    import quotes.reflect.*
    def error = report.errorAndAbort(
      s"Cannot derive FromExpr for ${TypeRepr.of[A].show}. Only case classes, sealed traits, and enums are supported"
    )
    Expr.summon[Mirror.Of[A]].getOrElse(error) match
      case '{ $m: Mirror.ProductOf[A] } => deriveProductImpl[A]
      case '{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } } =>
        val cases = TypeRepr.of[types].tupleToList
        deriveSumImpl[A](cases)

  def deriveProductImpl[A: Type](using Quotes): Expr[FromExpr[A]] =
    import quotes.reflect.*

    def makeMatch(expr: Expr[Expr[A]], quotes: Expr[Quotes]) =
      val unapply = deriveProductCaseDef[A](quotes)
      Match(expr.asTerm, List(unapply)).asExprOf[Option[A]]

    val ex = '{
      new FromExpr[A]:
        def unapply(expr: Expr[A])(using quotes: Quotes): Option[A] =
          import quotes.reflect.*
          try ${ makeMatch('expr, 'quotes) }
          catch
            case ex: MatchError =>
              report.warning(s"Could not extract value for ${expr.show}")
              None

    }

    ex.asInstanceOf[Expr[FromExpr[A]]]
  end deriveProductImpl

  def deriveSumImpl[A: Type](using Quotes)(cases: List[quotes.reflect.TypeRepr]): Expr[FromExpr[A]] =
    import quotes.reflect.*

    def makeMatch(expr: Expr[Expr[A]], quotes: Expr[Quotes]) =
      val caseDefs = cases.map { t =>
        t.asType match
          case '[t] =>
            deriveProductCaseDef[t](quotes)
      }
      Match(expr.asTerm, caseDefs).asExprOf[Option[A]]

    val ex = '{
      new FromExpr[A]:
        def unapply(expr: Expr[A])(using quotes: Quotes): Option[A] =
          import quotes.reflect.*
          try ${ makeMatch('expr, 'quotes) }
          catch
            case ex: MatchError =>
              report.warning(s"Could not extract value for ${expr.show}")
              None

    }

    ex.asInstanceOf[Expr[FromExpr[A]]]
  end deriveSumImpl

  private def deriveProductCaseDef[A: Type](quoteExpr: Expr[Quotes])(using Quotes): quotes.reflect.CaseDef =
    import quotes.reflect.*

    val exprUnapply: Term = '{ Expr }.asTerm.selectUnique("unapply")
    val fields            = TypeTree.of[A].symbol.caseFields
    val fieldTypes        = fields.map(_.returnType.asType)

    val exprMatchTerm = '{ $quoteExpr.asInstanceOf[quoted.runtime.QuoteMatching].ExprMatch }.asTerm

    val fieldExprTypes     = fieldTypes.map { case '[t] => TypeRepr.of[Expr[t]] }
    val fieldExprTupleType = TypeRepr.makeTuple(fieldExprTypes)

    val tupleCompanion =
      fields.length match
        case 0 => Symbol.classSymbol("EmptyTuple").companionModule
        case n => Symbol.classSymbol("scala.Tuple" + n).companionModule

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
        .appliedTo(quoteExpr.asTerm)

    val (binds, unapplyTerms) = fields.map { field =>
      val fieldType  = field.returnType
      val name       = freshName(field.name)
      val bindSymbol = Symbol.newBind(Symbol.spliceOwner, name, Flags.EmptyFlags, fieldType)

      val fromExpr = field.returnType.asType match
        case '[t] =>
          Expr.summon[FromExpr[t]].getOrElse {
            report.errorAndAbort(s"Cannot summon FromExpr[${field.returnType.show}]")
          }

      bindSymbol -> Unapply(
        exprUnapply.appliedToType(fieldType),
        List(fromExpr.asTerm, quoteExpr.asTerm),
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
