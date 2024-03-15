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
