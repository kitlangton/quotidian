package quotidian

import scala.quoted.*

object Debug:

  inline def debug[A](inline expr: A): A = ${ debugImpl('expr) }

  def debugImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    val typeRepr       = TypeRepr.of[A]
    val showTypeRepr   = typeRepr.show
    val prettyTypeRepr = pprint(typeRepr).toString
    val showExpr       = expr.show

    val prettyTerm = pprint(expr.asTerm.underlyingArgument).toString
    val symbol     = typeRepr.typeSymbol
    val info       = SymbolInfo.fromSymbol(symbol)
    val message =
      s"""${"EXPR".blue.underlined}
        |$showExpr
        |
        |${"TERM".blue.underlined}
        |$prettyTerm
        |
        |${pprint(info)}
        |""".stripMargin
    report.errorAndAbort(message)
    expr

  inline def debugWithType[A](inline expr: A): A = ${ debugWithTypeImpl('expr) }

  def debugWithTypeImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    val typeRepr       = TypeRepr.of[A]
    val showTypeRepr   = typeRepr.show
    val prettyTypeRepr = pprint(typeRepr).toString
    val showExpr       = expr.show
    val prettyTerm     = pprint(expr.asTerm.underlyingArgument).toString
    val message =
      s"""${"EXPR".blue.underlined}
        |$showExpr
        |
        |${"TERM".blue.underlined}
        |$prettyTerm
        |
        |${"TYPE".blue.underlined}
        |$showTypeRepr
        |
        |${"TYPE REPR".blue.underlined}
        |$prettyTypeRepr
        |""".stripMargin
    report.errorAndAbort(message)
    expr

  extension (self: String) //
    private def blue       = Console.BLUE + self + Console.RESET
    private def red        = Console.RED + self + Console.RESET
    private def cyan       = Console.CYAN + self + Console.RESET
    private def bold       = Console.BOLD + self + Console.RESET
    private def underlined = Console.UNDERLINED + self + Console.RESET

// def owner: Symbol
//
//        /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
//        def maybeOwner: Symbol
//
//        /** Flags of this symbol */
//        def flags: Flags
//
//        /** This symbol is private within the resulting type */
//        def privateWithin: Option[TypeRepr]
//
//        /** This symbol is protected within the resulting type */
//        def protectedWithin: Option[TypeRepr]
//
//        /** The name of this symbol */
//        def name: String
//
//        /** The full name of this symbol up to the root package */
//        def fullName: String
//
//        /** The position of this symbol */
//        def pos: Option[Position]
//
//        /** The documentation for this symbol, if any */
//        def docstring: Option[String]
//
//        /** Tree of this definition
//         *
//         *  If this symbol `isClassDef` it will return `a `ClassDef`,
//         *  if this symbol `isTypeDef` it will return `a `TypeDef`,
//         *  if this symbol `isValDef` it will return `a `ValDef`,
//         *  if this symbol `isDefDef` it will return `a `DefDef`
//         *  if this symbol `isBind` it will return `a `Bind`,
//         *  else will throw
//         *
//         *  **Warning**: avoid using this method in macros.
//         *
//         *  **Caveat**: The tree is not guaranteed to exist unless the compiler
//         *  option `-Yretain-trees` is enabled.
//         *
//         *  **Anti-pattern**: The following code is an anti-pattern:
//         *
//         *      symbol.tree.tpe
//         *
//         *  It should be replaced by one of the following:
//         *
//         *      tp.memberType(symbol)
//         *      symbol.typeRef
//         *      symbol.termRef
//         *
//         */
//        def tree: Tree
//
//        /** Is the annotation defined with `annotSym` attached to this symbol? */
//        def hasAnnotation(annotSym: Symbol): Boolean
//
//        /** Get the annotation defined with `annotSym` attached to this symbol */
//        def getAnnotation(annotSym: Symbol): Option[Term]
//
//        /** Annotations attached to this symbol */
//        def annotations: List[Term]
//
//        /** Does this symbol come from a currently compiled source file? */
//        def isDefinedInCurrentRun: Boolean
//
//        /** Dummy val symbol that owns all statements within the initialization of the class.
//        *  This may also contain local definitions such as classes defined in a `locally` block in the class.
//        */
//        def isLocalDummy: Boolean
//
//        /** Is this symbol a class representing a refinement? */
//        def isRefinementClass: Boolean
//
//        /** Is this symbol an alias type? */
//        def isAliasType: Boolean
//
//        /** Is this symbol an anonymous class? */
//        def isAnonymousClass: Boolean
//
//        /** Is this symbol an anonymous function? */
//        def isAnonymousFunction: Boolean
//
//        /** Is this symbol an abstract type? */
//        def isAbstractType: Boolean
//
//        /** Is this the constructor of a class? */
//        def isClassConstructor: Boolean
//
//        /** Is this the definition of a type? */
//        def isType: Boolean
//
//        /** Is this the definition of a term? */
//        def isTerm: Boolean
//
//        /** Is this the definition of a PackageDef tree? */
//        def isPackageDef: Boolean
//
//        /** Is this the definition of a ClassDef tree? */
//        def isClassDef: Boolean
//
//        /** Is this the definition of a TypeDef tree */
//        def isTypeDef: Boolean
//
//        /** Is this the definition of a ValDef tree? */
//        def isValDef: Boolean
//
//        /** Is this the definition of a DefDef tree? */
//        def isDefDef: Boolean
//
//        /** Is this the definition of a Bind pattern? */
//        def isBind: Boolean
//
//        /** Does this symbol represent a no definition? */
//        def isNoSymbol: Boolean
//
//        /** Does this symbol represent a definition? */
//        def exists: Boolean
//
//        /** Field with the given name directly declared in the class */
//        def declaredField(name: String): Symbol
//
//        /** Fields directly declared in the class */
//        def declaredFields: List[Symbol]
//
//        /** Get named non-private fields declared or inherited */
//        @deprecated("Use fieldMember", "3.1.0")
//        def memberField(name: String): Symbol
//
//        /** Get named non-private fields declared or inherited */
//        def fieldMember(name: String): Symbol
//
//        /** Get all non-private fields declared or inherited */
//        @deprecated("Use fieldMembers", "3.1.0")
//        def memberFields: List[Symbol]
//
//        /** Get all non-private fields declared or inherited */
//        def fieldMembers: List[Symbol]
//
//        /** Get non-private named methods defined directly inside the class */
//        def declaredMethod(name: String): List[Symbol]
//
//        /** Get all non-private methods defined directly inside the class, excluding constructors */
//        def declaredMethods: List[Symbol]
//
//        /** Get named non-private methods declared or inherited */
//        @deprecated("Use methodMember", "3.1.0")
//        def memberMethod(name: String): List[Symbol]
//
//        /** Get named non-private methods declared or inherited */
//        def methodMember(name: String): List[Symbol]
//
//        /** Get all non-private methods declared or inherited */
//        @deprecated("Use methodMembers", "3.1.0")
//        def memberMethods: List[Symbol]
//
//        /** Get all non-private methods declared or inherited */
//        def methodMembers: List[Symbol]
//
//        /** Get non-private named type defined directly inside the class */
//        def declaredType(name: String): List[Symbol]
//
//        /** Get all non-private types defined directly inside the class */
//        def declaredTypes: List[Symbol]
//
//        /** Type member with the given name directly declared in the class */
//        @deprecated("Use typeMember", "3.1.0")
//        def memberType(name: String): Symbol
//
//        /** Type member with the given name directly declared in the class */
//        def typeMember(name: String): Symbol
//
//        /** Type member directly declared in the class */
//        @deprecated("Use typeMembers", "3.1.0")
//        def memberTypes: List[Symbol]
//
//        /** Type member directly declared in the class */
//        def typeMembers: List[Symbol]
//
//        /** All members directly declared in the class */
//        def declarations: List[Symbol]
//
//        /** The symbols of each type parameter list and value parameter list of this
//          *  method, or Nil if this isn't a method.
//          */
//        def paramSymss: List[List[Symbol]]
//
//        /** Returns all symbols overridden by this symbol. */
//        def allOverriddenSymbols: Iterator[Symbol]
//
//        /** The symbol overriding this symbol in given subclass `ofclazz`.
//         *
//         *  @param ofclazz is a subclass of this symbol's owner
//         */
//        def overridingSymbol(ofclazz: Symbol): Symbol
//
//        /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
//        def primaryConstructor: Symbol
//
//        /** Fields of a case class type -- only the ones declared in primary constructor */
//        def caseFields: List[Symbol]
//
//        def isTypeParam: Boolean
//
//        /** Signature of this definition */
//        def signature: Signature
//
//        /** The class symbol of the companion module class */
//        def moduleClass: Symbol
//
//        /** The symbol of the companion class */
//        def companionClass: Symbol
//
//        /** The symbol of the companion module */
//        def companionModule: Symbol
//
//        /** Case class or case object children of a sealed trait or cases of an `enum`. */
//        def children: List[Symbol]
//
//        /** Returns a nested quote with this symbol as splice owner (`Symbol.spliceOwner`).
//         *
//         *  Changes the owner under which the definition in a quote are created.
//         *
//         *  Usages:
//         *  ```scala
//         *  def rhsExpr(using q: Quotes): Expr[Unit] =
//         *    import q.reflect._
//         *    '{ val y = ???; (y, y) }
//         *  def aValDef(using q: Quotes)(owner: q.reflect.Symbol) =
//         *    import q.reflect._
//         *    val sym = Symbol.newVal(owner, "x", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)
//         *    val rhs = rhsExpr(using sym.asQuotes).asTerm
//         *    ValDef(sym, Some(rhs))
//         *  ```
//         *
//         *  ```scala
//         *  //{
//         *  def inQuotes(using q: Quotes) = {
//         *    import q.reflect._
//         *  //}
//         *    new TreeMap:
//         *      override def transformTerm(tree: Term)(owner: Symbol): Term =
//         *        tree match
//         *          case tree: Ident =>
//         *            given Quotes = owner.asQuotes
//         *            // Definitions contained in the quote will be owned by `owner`.
//         *            // No need to use `changeOwner` in this case.
//         *            '{ val x = ???; x }.asTerm
//         *  //{
//         *  }
//         *  //}
//         *  ```
//         */
//        def asQuotes: Nested
final case class SymbolInfo(
    fullName: String,
    flags: String,
    docString: Option[String],
    annotations: List[String],
    declaredFields: List[String],
//    inheritedFields: List[String],
    declaredMethods: List[String],
//    methodMembers: List[String],
    declaredTypes: List[String],
    overridingSymbols: List[String] = Nil,
    companionClass: String,
    companionModule: String,
    children: List[String]
)

object SymbolInfo:
  def fromSymbol(using Quotes)(symbol: quotes.reflect.Symbol): SymbolInfo =
    SymbolInfo(
      symbol.fullName,
      symbol.flags.show,
      symbol.docstring,
      symbol.annotations.map(_.show),
      symbol.declaredFields.map(_.fullName),
//      (symbol.fieldMembers diff symbol.declaredFields).map(_.fullName),
      symbol.declaredMethods.map(_.fullName),
//      symbol.methodMembers.map(_.fullName),
      symbol.declaredTypes.map(_.fullName),
      symbol.allOverriddenSymbols.map(_.fullName).toList,
      symbol.companionClass.fullName,
      symbol.companionModule.fullName,
      symbol.children.map(_.fullName)
    )
