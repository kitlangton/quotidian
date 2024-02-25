package quotidian.syntax

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.ClassTag
import scala.annotation.targetName

/////////////////////
// Expr Extensions //
/////////////////////

extension (self: Expr.type)

  /** Packs the given expressions into an Expr of an array.
    *
    * @example
    *   {{{
    * val result: Expr[Array[String]] = Expr.ofArray(Expr("Bongo"), Expr("Dingo"))
    * // '{ Array("Bongo", "Dingo") }
    *   }}}
    */
  def ofArray[A: Type](using Quotes)(as: Expr[A]*): Expr[Array[A]] =
    '{ Array(${ Expr.ofSeq(as) }*)(using ${ Expr.summon[ClassTag[A]].get }) }

  def ofMap[K: Type, V: Type](using Quotes)(as: (Expr[K], Expr[V])*): Expr[Map[K, V]] =
    '{ Map(${ Expr.ofSeq(as.map { case (k, v) => '{ $k -> $v } }) }*) }

  def ofMap[V: Type](using Quotes)(as: Seq[(String, Expr[V])]): Expr[Map[String, V]] =
    '{ Map(${ Expr.ofSeq(as.map { case (k, v) => '{ ${ Expr(k) } -> $v } }) }*) }

  /** Creates an interpolated String Expr from the given String literals and
    * Exprs.
    *
    * @example
    *   {{{
    * val nameExpr = Expr("Kit")
    * val intExpr = Expr(33)
    *
    * val result: Expr[String] =
    *   interpolatedString("My name is ", nameExpr, " and I am ", intExpr, " years old")
    * // '{ s"My name is $nameExpr and I am $intExpr years old" }
    *   }}}
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

extension [A: Type](using Quotes)(self: Expr[A])
  def cast[To: Type]: Expr[To] =
    '{ $self.asInstanceOf[To] }

///////////////////////////
// Refinement Extensions //
///////////////////////////
extension (using Quotes)(self: quotes.reflect.Refinement.type)
  def of(
      base: quotes.reflect.TypeRepr, //
      refinements: Seq[(String, quotes.reflect.TypeRepr)]
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    refinements.foldLeft(base) { case (acc, (name, tpe)) =>
      Refinement(acc, name, tpe)
    }

  def of[Base: Type](
      refinements: Seq[(String, quotes.reflect.TypeRepr)]
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    Refinement.of(TypeRepr.of[Base], refinements)

/////////////////////////
// TypeRepr Extensions //
/////////////////////////

extension (using Quotes)(self: quotes.reflect.TypeRepr.type)

  def fieldTypes[A: Type]: List[quotes.reflect.TypeRepr] =
    Expr.summon[Mirror.ProductOf[A]].get match
      case '{ $p: Mirror.ProductOf[A] { type MirroredElemTypes = tpes } } =>
        quotes.reflect.TypeRepr.of[tpes].tupleToList

  def makeTuple(args: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val tupleCons = TypeRepr.of[*:]
    args
      .foldRight(TypeRepr.of[EmptyTuple])((tpe, acc) => tupleCons.appliedTo(List(tpe, acc)))

  def makeTupleClass(args: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val tupleClassType = defn.TupleClass(args.length)
    if tupleClassType.isNoSymbol then report.errorAndAbort(s"Tuple${args.length} elements does not exist")
    tupleClassType.typeRef.appliedTo(args)

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

/////////////////////
// Type Extensions //
/////////////////////

extension (using Quotes)(self: Type.type)
  def valueAs[A: Type, B]: B =
    import quotes.reflect.*
    Type.valueOfConstant[A].get.asInstanceOf[B]

///////////////////////
// Symbol Extensions //
///////////////////////

extension (using Quotes)(self: quotes.reflect.Symbol.type)
  def of[A: Type]: quotes.reflect.Symbol =
    import quotes.reflect.*
    TypeTree.of[A].symbol

extension (using Quotes)(self: quotes.reflect.Symbol)

  def paramTypes: List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    self.paramSymss.flatten.map(_.termRef.widenTermRefByName)

  def returnType: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    self.termRef.widenTermRefByName match
      case MethodType(_, _, res) => res
      case other                 => other

  def isPublic: Boolean =
    import quotes.reflect.*
    !self.flags.is(Flags.Private) && !self.flags.is(Flags.Protected) &&
    !self.flags.is(Flags.Local) && !self.flags.is(Flags.Synthetic) &&
    !self.flags.is(Flags.Artifact) && !self.flags.is(Flags.Macro)

/////////////////////
// Term Extensions //
/////////////////////

extension (using Quotes)(self: quotes.reflect.Term.type)
  def companionOf[A: Type]: quotes.reflect.Term =
    import quotes.reflect.*
    Term.companionOf(TypeRepr.of[A])

  def companionOf(tpe: quotes.reflect.TypeRepr): quotes.reflect.Term =
    import quotes.reflect.*
    Ident(tpe.typeSymbol.companionModule.termRef)

extension (using Quotes)(self: quotes.reflect.Term)
  def selectUnique(name: String): quotes.reflect.Term =
    import quotes.reflect.*
    Select.unique(self, name)

  def selectUnique(symbol: quotes.reflect.Symbol): quotes.reflect.Term =
    import quotes.reflect.*
    Select(self, symbol)

  def selectOverloaded(
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    Select.overloaded(self, name, targs, args)

  def selectOverloaded(
      symbol: quotes.reflect.Symbol,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    Select(self, symbol).appliedToTypes(targs).appliedToArgs(args)

  def copy(labeledValues: (String, quotes.reflect.Term)*): quotes.reflect.Term =
    import quotes.reflect.*
    val namedArgs = labeledValues.map((name, value) => NamedArg(name, value)).toList
    Select.overloaded(self, "copy", self.tpe.typeArgs, namedArgs.toList)

  def call(name: String): quotes.reflect.Term =
    TermUtils.callImpl(self, name, List.empty[quotes.reflect.TypeRepr], List.empty[quotes.reflect.Term])

  def call(symbol: quotes.reflect.Symbol): quotes.reflect.Term =
    TermUtils.callSymbolImpl(self, symbol, List.empty[quotes.reflect.TypeRepr], List.empty[quotes.reflect.Term])

  def call(name: String, args: List[quotes.reflect.Term]): quotes.reflect.Term =
    TermUtils.callImpl(self, name, List.empty[quotes.reflect.TypeRepr], args)

  def call(symbol: quotes.reflect.Symbol, args: List[quotes.reflect.Term]): quotes.reflect.Term =
    TermUtils.callSymbolImpl(self, symbol, List.empty[quotes.reflect.TypeRepr], args)

  def callType(symbol: quotes.reflect.Symbol, targs: List[quotes.reflect.TypeRepr]): quotes.reflect.Term =
    TermUtils.callSymbolImpl(self, symbol, targs, List.empty[quotes.reflect.Term])

  def call(
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    TermUtils.callImpl(self, name, targs, args)

  def call(
      symbol: quotes.reflect.Symbol,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    TermUtils.callSymbolImpl(self, symbol, targs, args)

private object TermUtils:
  def callImpl(using Quotes)(
      self: quotes.reflect.Term,
      name: String,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*
    // TODO: Test & fix this to work against methods with multiple overloads
    val symbol = self.symbol.methodMember(name).headOption.getOrElse(self.symbol.fieldMember(name))
    callSymbolImpl(self, symbol, targs, args)

  def callSymbolImpl(using Quotes)(
      self: quotes.reflect.Term,
      symbol: quotes.reflect.Symbol,
      targs: List[quotes.reflect.TypeRepr],
      args: List[quotes.reflect.Term]
  ): quotes.reflect.Term =
    import quotes.reflect.*

    symbol.paramSymss match
      case Nil => self.selectUnique(symbol)

      // we can attempt to infer the type args if they are not provided:
      // - Zip the params with the args.
      // - If the arg is a type param, we can associate the arg's type with the type param
      // - Use the inferred type args to call the method
      case (typeParams @ a :: _) :: allParams if a.isTypeParam && targs.isEmpty =>
        val params        = allParams.flatten
        val inferredTargs = inferTypeArgs(params, args)
        val concreteTypes = typeParams.flatMap(inferredTargs.get(_))
        if params.isEmpty then Select(self, symbol).appliedToTypes(concreteTypes)
        else self.selectOverloaded(symbol, concreteTypes, args)

      case (typeParams @ a :: _) :: params if a.isTypeParam =>
        if params.isEmpty then Select(self, symbol).appliedToTypes(targs)
        else self.selectOverloaded(symbol, targs, args)

      case other =>
        self.selectOverloaded(symbol, targs, args)

  def inferTypeArgs(using Quotes)(
      termParams: List[quotes.reflect.Symbol],
      args: List[quotes.reflect.Term]
  ): Map[quotes.reflect.Symbol, quotes.reflect.TypeRepr] =
    termParams
      .zip(args)
      .flatMap {
        case (param, arg) if param.termRef.widenTermRefByName.typeSymbol.isTypeParam =>
          Some(param.termRef.widenTermRefByName.typeSymbol -> arg.tpe)
        case _ => None
      }
      .toMap

// Extractors

object Uninlined:
  @tailrec
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, _, t) => Uninlined.unapply(t)
      case t                => Some(t)
