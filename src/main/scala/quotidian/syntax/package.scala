package quotidian.syntax

import scala.annotation.tailrec
import scala.quoted.*

// Extensions

extension (using Quotes)(symbol: quotes.reflect.Symbol.type)
  def of[A: Type]: quotes.reflect.Symbol =
    import quotes.reflect.*
    TypeTree.of[A].symbol

extension (using Quotes)(symbol: quotes.reflect.Term.type)
  def companionOf[A: Type]: quotes.reflect.Term =
    import quotes.reflect.*
    Ident(Symbol.of[A].companionModule.termRef)

extension (using Quotes)(symbol: quotes.reflect.TypeRepr.type)

  def makeTuple(args: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val tupleCons = TypeRepr.typeConstructor[*:[?, ?]]
    args
      .foldRight(TypeRepr.of[EmptyTuple]) { (tpe, acc) =>
        tpe.asType match
          case '[t] =>
            AppliedType(tupleCons, List(TypeRepr.of[Expr[t]], acc))
      }

  def typeConstructor[A: Type]: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val typeRepr = TypeRepr.of[A]
    typeRepr match
      case UnderlyingTypeConstructor(t) => t
      case _                            => report.errorAndAbort(s"Expected a type constructor, but got ${typeRepr.show}")

extension (using Quotes)(symbol: quotes.reflect.Symbol)
  def returnType: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    symbol.termRef.widenTermRefByName

  def isPublic: Boolean =
    import quotes.reflect.*
    !symbol.flags.is(Flags.Private) && !symbol.flags.is(Flags.Protected) &&
    !symbol.flags.is(Flags.Local) && !symbol.flags.is(Flags.Synthetic) &&
    !symbol.flags.is(Flags.Artifact) && !symbol.flags.is(Flags.Macro)

extension (using Quotes)(term: quotes.reflect.Term)
  def uninline: quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, term) => term.uninline
      case _                   => term

  def selectUnique(name: String): quotes.reflect.Term =
    import quotes.reflect.*
    Select.unique(term, name)

  def selectOverloaded(
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    Select.overloaded(term, name, targs, args)

extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
  def typeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    tpe.asType match
      case '[t] => TypeTree.of[t]

  /** Turn a tuple of a TypeRepr into a List[TypeRepr]
    */
  def tupleToList: List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tpe.asType match
      case '[t *: ts]    => TypeRepr.of[t] :: TypeRepr.of[ts].tupleToList
      case '[EmptyTuple] => Nil

extension [A](using Quotes)(expr: Expr[A])
  /** Returns the underlying function term of a function application.
    */
  def underlyingFunction: quotes.reflect.Term =
    import quotes.reflect.*
    expr.asTerm match
      case UnderlyingFunction(term) =>
        println(s"Underlying function: ${term}")
        term
      case _ => report.errorAndAbort(s"Expected a function application, but got ${expr.show}")

// Extractors

object Uninlined:
  @tailrec
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, _, t) => Uninlined.unapply(t)
      case t                => Some(t)

object UnderlyingFunction:
  /** Extracts the underlying function term of a function application.
    */
  @tailrec
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, _, t) => UnderlyingFunction.unapply(t)
      case Apply(t, _)      => UnderlyingFunction.unapply(t)
      case TypeApply(t, _)  => UnderlyingFunction.unapply(t)
      case t                => Some(t)

object UnderlyingTypeConstructor:
  /** Extracts the underlying function term of a function application.
    */
  @tailrec
  def unapply(using Quotes)(term: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    term match
      case AppliedType(t, _) => UnderlyingTypeConstructor.unapply(t)
      case t                 => Some(t)
