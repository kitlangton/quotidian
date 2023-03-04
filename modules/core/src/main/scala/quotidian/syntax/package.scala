package quotidian.syntax

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.ClassTag

// Extensions

extension (self: Expr.type)

  def ofArray[A: Type](using Quotes)(as: Expr[A]*): Expr[Array[A]] =
    '{ Array(${ Expr.ofSeq(as) }*)(using ${ Expr.summon[ClassTag[A]].get }) }

  /** Creates an interpolated String Expr from a list of strings and expressions.
    */
  def interpolatedString(using Quotes)(as: (String | Expr[?])*): Expr[String] =
    import quotes.reflect.*
    val grouped = as.toList.foldRight(List.empty) {
      case (s: String, Nil)              => List(s)
      case (s: String, (h: String) :: t) => (s + h) :: t
      case (a, acc)                      => a :: acc
    }
    val parts = Expr.ofSeq(grouped.collect { case s: String => Expr(s) })
    val args  = Expr.ofSeq(grouped.collect { case e: Expr[?] => e })
    '{ StringContext($parts*).s($args*) }

extension (using Quotes)(self: quotes.reflect.Symbol.type)
  def of[A: Type]: quotes.reflect.Symbol =
    import quotes.reflect.*
    TypeTree.of[A].symbol

extension (using Quotes)(self: quotes.reflect.Term.type)
  def companionOf[A: Type]: quotes.reflect.Term =
    import quotes.reflect.*
    Term.companionOf(TypeRepr.of[A])

  def companionOf(tpe: quotes.reflect.TypeRepr): quotes.reflect.Term =
    import quotes.reflect.*
    Ident(tpe.typeSymbol.companionModule.termRef)

extension (using Quotes)(self: quotes.reflect.TypeRepr.type)

  def fieldTypes[A: Type]: List[quotes.reflect.TypeRepr] =
    Expr.summon[Mirror.ProductOf[A]].get match
      case '{ $p: Mirror.ProductOf[A] { type MirroredElemTypes = tpes } } =>
        quotes.reflect.TypeRepr.of[tpes].tupleToList

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

extension (using Quotes)(self: quotes.reflect.Symbol)
  def returnType: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    self.termRef.widenTermRefByName

  def isPublic: Boolean =
    import quotes.reflect.*
    !self.flags.is(Flags.Private) && !self.flags.is(Flags.Protected) &&
    !self.flags.is(Flags.Local) && !self.flags.is(Flags.Synthetic) &&
    !self.flags.is(Flags.Artifact) && !self.flags.is(Flags.Macro)

extension (using Quotes)(self: quotes.reflect.Term)
  def selectUnique(name: String): quotes.reflect.Term =
    import quotes.reflect.*
    Select.unique(self, name)

  def selectOverloaded(
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    Select.overloaded(self, name, targs, args)

  def call(name: String): quotes.reflect.Term =
    TermUtils.callImpl(self, name, List.empty[quotes.reflect.TypeRepr], List.empty[quotes.reflect.Term])

  def call(name: String, args: List[quotes.reflect.Term]): quotes.reflect.Term =
    TermUtils.callImpl(self, name, List.empty[quotes.reflect.TypeRepr], args)

  def call(
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term] = List.empty
  ): quotes.reflect.Term =
    TermUtils.callImpl(self, name, targs, args)

object TermUtils:
  def callImpl(using Quotes)(
      self: quotes.reflect.Term,
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    // TODO: Test & fix this to work against methods with multiple overloads
    val symbol = self.symbol.methodMember(name).headOption.getOrElse(self.symbol.fieldMember(name))
    symbol.paramSymss match
      case Nil => self.selectUnique(name)
      case _   => self.selectOverloaded(name, targs, args)

extension (using Quotes)(self: quotes.reflect.TypeRepr)
  def unapplied: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    self match
      case AppliedType(t, _) => t.unapplied
      case _                 => self

  def valueAs[A]: A =
    import quotes.reflect.*
    self.asType match
      case '[t] => Type.valueOfConstant[t].get.asInstanceOf[A]
      case _    => report.errorAndAbort(s"Expected a literal, but got ${self.show}")

  def isGeneric: Boolean =
    import quotes.reflect.*
    self.typeSymbol.isTypeParam

  def typeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    self.asType match
      case '[t] => TypeTree.of[t]

  /** Turn a tuple of a TypeRepr into a List[TypeRepr]
    */
  def tupleToList: List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    self.asType match
      case '[t *: ts]    => TypeRepr.of[t] :: TypeRepr.of[ts].tupleToList
      case '[EmptyTuple] => Nil

//extension [A](using Quotes)(self: Expr[A])

// Extractors

object Uninlined:
  @tailrec
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, _, t) => Uninlined.unapply(t)
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
