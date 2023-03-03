package quotidian

import scala.quoted.*
import scala.deriving.Mirror
import quotidian.syntax.*

class MirrorElem[Q <: Quotes & Singleton, A](using val quotes: Q, val asType: Type[A])(
    val label: String,
    val typeRepr: quotes.reflect.TypeRepr
):
  import quotes.*

  def summon[F[_]: Type]: Expr[F[A]] =
    val repr = quotes.reflect.TypeRepr.of[F[A]]
    Expr
      .summon[F[A]]
      .getOrElse(quotes.reflect.report.errorAndAbort(s"Cannot summon ${repr.show}"))

  def valueOfConstant: Option[A] =
    Type.valueOfConstant[A]

sealed trait MacroMirror[Q <: Quotes & Singleton, A]:
  val quotes: Q
  val tpe: Type[A]

  import quotes.reflect.*

  def label: String
  def monoType: quotes.reflect.TypeRepr
  def elemLabels: List[String]
  def elemTypes: List[quotes.reflect.TypeRepr]

  def elems: List[MirrorElem[quotes.type, ?]] =
    given Quotes = quotes
    elemLabels.zip(elemTypes).map { case (label, elemType) =>
      elemType.asType match
        case '[t] =>
          new MirrorElem[quotes.type, t](using quotes)(label = label, typeRepr = elemType)
    }

  def summonAll[F[_]: Type]: List[Expr[F[Any]]] =
    elems.map(_.summon[F]).asInstanceOf[List[Expr[F[Any]]]]

  protected def mirrorTypeLabel: String =
    this match
      case _: MacroMirror.SingletonMacroMirror[Q, A] => "Singleton"
      case _: MacroMirror.ProductMacroMirror[Q, A]   => "Product"
      case _: MacroMirror.SumMacroMirror[Q, A]       => "Sum"

  override def toString: String =
    s"""${mirrorTypeLabel}MacroMirror(
       |  label = $label,
       |  monoType = ${monoType.show}
       |  elemLabels = $elemLabels
       |  elemTypes = ${elemTypes.map(_.show)}
       |)""".stripMargin

object MacroMirror:

  abstract class SumMacroMirror[Q <: Quotes & Singleton, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def ordinal[T: Type]: Int =
      ordinal(TypeRepr.of[T])

    def ordinal(repr: TypeRepr): Int =
      elemTypes.indexWhere(repr =:= _) match
        case -1 => report.errorAndAbort(s"Type $repr is not a member of $monoType")
        case n  => n

  abstract class ProductMacroMirror[Q <: Quotes & Singleton, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def construct(args: Seq[quotes.reflect.Term]): Expr[A] =
      Term
        .companionOf(monoType)
        .call("apply", monoType.typeArgs, args.toList)
        .asExprOf[A]

  abstract class SingletonMacroMirror[Q <: Quotes & Singleton, A](using
      override val quotes: Q,
      override val tpe: Type[A]
  ) extends ProductMacroMirror[Q, A]:
    import quotes.reflect.*

    override def construct(args: Seq[quotes.reflect.Term]): Expr[A] =
      expr

    def expr: Expr[A] =
      Ref(monoType.termSymbol).asExprOf[A]

  def summon[A: Type](using quotes: Quotes): Either[String, MacroMirror[quotes.type, A]] =
    import quotes.reflect.*
    Expr
      .summon[Mirror.Of[A]] match

      case Some('{
            $m: Mirror.Singleton {
              type MirroredMonoType   = monoType
              type MirroredLabel      = label
              type MirroredElemTypes  = elemTypes
              type MirroredElemLabels = elemLabels
            }
          }) =>
        val mirror = new SingletonMacroMirror[quotes.type, A]:
          val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
          val monoType   = TypeRepr.of[monoType]
          val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
          val elemTypes  = TypeRepr.of[elemTypes].tupleToList
        Right(mirror)

      case Some('{
            $m: Mirror.ProductOf[A] {
              type MirroredMonoType   = monoType
              type MirroredLabel      = label
              type MirroredElemTypes  = elemTypes
              type MirroredElemLabels = elemLabels
            }
          }) =>
        val mirror = new ProductMacroMirror[quotes.type, A]:
          val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
          val monoType   = TypeRepr.of[monoType]
          val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
          val elemTypes  = TypeRepr.of[elemTypes].tupleToList
        Right(mirror)

      case Some('{
            $m: Mirror.SumOf[A] {
              type MirroredMonoType   = monoType
              type MirroredLabel      = label
              type MirroredElemTypes  = elemTypes
              type MirroredElemLabels = elemLabels
            }
          }) =>
        val mirror = new SumMacroMirror[quotes.type, A]:
          val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
          val monoType   = TypeRepr.of[monoType]
          val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
          val elemTypes  = TypeRepr.of[elemTypes].tupleToList
        Right(mirror)
      case _ =>
        Left(s"Cannot summon Mirror.Of[${TypeRepr.of[A].show}]")
  end summon

  def summonProduct[A: Type](using quotes: Quotes): ProductMacroMirror[quotes.type, A] =
    import quotes.reflect.*
    summon[A] match
      case Right(m: ProductMacroMirror[quotes.type, A]) => m
      case _                                     => quotes.reflect.report.errorAndAbort(s"Cannot summon ProductMacroMirror[${TypeRepr.of[A].show}]")
