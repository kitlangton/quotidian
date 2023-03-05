package quotidian

import scala.quoted.*
import scala.deriving.Mirror
import quotidian.syntax.*

class MirrorElem[Q <: Quotes, M, A](using val quotes: Q, val asType: Type[A])(
    val label: String,
    val typeRepr: quotes.reflect.TypeRepr,
    val symbol: quotes.reflect.Symbol
):
  import quotes.reflect.*

  def get(parent: Expr[M]): Expr[A] =
    dereference(parent.asTerm).asExprOf[A]

  def dereference(parent: quotes.reflect.Term): quotes.reflect.Term =
    parent.selectUnique(label)

  def ==(that: MirrorElem[?, ?, ?]) = symbol == that.symbol

  def summon[F[_]: Type]: Expr[F[A]] =
    val repr = quotes.reflect.TypeRepr.of[F[A]]
    Expr
      .summon[F[A]]
      .getOrElse(quotes.reflect.report.errorAndAbort(s"Cannot summon ${repr.show}"))

  def valueOfConstant: Option[A] =
    Type.valueOfConstant[A]

sealed trait MacroMirror[Q <: Quotes, A]:
  val quotes: Q
  val tpe: Type[A]

  import quotes.reflect.*

  def label: String
  def monoType: quotes.reflect.TypeRepr
  def elemLabels: List[String]
  def elemTypeReprs: List[quotes.reflect.TypeRepr]
  def elemTypes: List[Type[?]] = elemTypeReprs.map(_.asType)

  def elems: List[MirrorElem[quotes.type, A, ?]] =
    given Quotes = quotes
    elemLabels.zip(elemTypeReprs).map { case (label, elemType) =>
      elemType.asType match
        case '[t] =>
          new MirrorElem[quotes.type, A, t](using quotes)(
            label = label,
            typeRepr = elemType,
            symbol = monoType.typeSymbol.fieldMember(label)
          )
    }

  def summonAll[F[_]: Type]: List[Expr[F[Any]]] =
    elems.map(_.summon[F]).asInstanceOf[List[Expr[F[Any]]]]

  /** Returns an Expr[Array[F[_]]], first trying to summon an instance of `F[_]`
    * for each element type, and then falling back to deriving an instance of
    * `F[_]` for each element type.
    *
    * `val instances = ${ mirror.deriveArray[Eq]([t] => () => deriveEq[t]) }`
    */
  def deriveArray[F[_]: Type](
      deriveFallback: [t] => () => Type[t] ?=> Expr[F[t]]
  )(using Quotes): Expr[Array[F[Any]]] =
    Expr.ofArray[F[Any]](
      elemTypes.map { case '[t] =>
        val fallback = deriveFallback()(using Type.of[t])
        '{ ${ Expr.summon[F[t]].getOrElse(fallback) }.asInstanceOf[F[Any]] }
      }*
    )

  protected def mirrorTypeLabel: String =
    this match
      case _: MacroMirror.Singleton[Q, A] => "Singleton"
      case _: MacroMirror.Product[Q, A]   => "Product"
      case _: MacroMirror.Sum[Q, A]       => "Sum"

  override def toString: String =
    s"""${mirrorTypeLabel}MacroMirror(
       |  label = $label,
       |  monoType = ${monoType.show}
       |  elemLabels = $elemLabels
       |  elemTypes = ${elemTypeReprs.map(_.show)}
       |)""".stripMargin

object MacroMirror:

  abstract class Sum[Q <: Quotes, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def mirrorExpr =
      Expr.summon[Mirror.SumOf[A]].get

    def ordinal[T: Type]: Int =
      ordinal(TypeRepr.of[T])

    def ordinal(repr: TypeRepr): Int =
      elemTypeReprs.indexWhere(repr =:= _) match
        case -1 => report.errorAndAbort(s"Type $repr is not a member of $monoType")
        case n  => n

  abstract class Product[Q <: Quotes, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def construct(args: Seq[quotes.reflect.Term]): Expr[A] =
      Term
        .companionOf(monoType)
        .call("apply", monoType.typeArgs, args.toList)
        .asExprOf[A]

  abstract class Singleton[Q <: Quotes, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends MacroMirror.Product[Q, A]:
    import quotes.reflect.*

    override def construct(args: Seq[quotes.reflect.Term]): Expr[A] =
      expr

    def expr: Expr[A] =
      Ref(monoType.termSymbol).asExprOf[A]

  def summon[A: Type](using quotes: Quotes): Option[MacroMirror[quotes.type, A]] =
    import quotes.reflect.*
    Expr
      .summon[Mirror.Of[A]]
      .collect {
        case '{
              $m: Mirror.Singleton {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new MacroMirror.Singleton[quotes.type, A]:
            val label         = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType      = TypeRepr.of[monoType]
            val elemLabels    = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypeReprs = TypeRepr.of[elemTypes].tupleToList

        case '{
              $m: Mirror.ProductOf[A] {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new MacroMirror.Product[quotes.type, A]:
            val label         = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType      = TypeRepr.of[monoType]
            val elemLabels    = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypeReprs = TypeRepr.of[elemTypes].tupleToList

        case '{
              $m: Mirror.SumOf[A] {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new Sum[quotes.type, A]:
            val label         = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType      = TypeRepr.of[monoType]
            val elemLabels    = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypeReprs = TypeRepr.of[elemTypes].tupleToList

      }
  end summon

  def summonProduct[A: Type](using quotes: Quotes): Product[quotes.type, A] =
    import quotes.reflect.*
    summon[A] match
      case Some(m: MacroMirror.Product[quotes.type, A]) => m
      case _                                            => quotes.reflect.report.errorAndAbort(s"Cannot summon ProductMacroMirror[${TypeRepr.of[A].show}]")
