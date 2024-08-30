package quotidian

import quotidian.StringUtils.*

import scala.quoted.*

object Report:
  def debug(using Quotes)(tree: quotes.reflect.Tree): Unit =
    import quotes.reflect.*
    val prettyTerm = pprint(tree).toString
    report.errorAndAbort {
      s"""
${"SHOW".blue.underlined}
${tree.show}

${"TREE".blue.underlined}
$prettyTerm
"""
    }

object Debug:

  inline def debug[A](inline expr: A): A = ${ debugImpl('expr) }

  // def prettyPrint(using Quotes)(tree: quotes.reflect.Tree): String =
  //   import quotes.reflect.*

  //   val b = new StringBuilder

  //   def render(tree: Tree, indent: Int = 0): Unit =
  //     tree match
  //       // Ident("name")
  //       case Ident(name) =>
  //         b.append(s"Ident($name)")

  //       case Select(tree, name) =>
  //         b.append(s"Select(${render(tree)}, $name)")

  //       case Apply(func, args) =>
  //         b.append(s"Apply(${render(func)}, ${args.map(render).mkString(", ")})")

  //       case ApplyInfix(left, op, right) =>
  //         b.append(s"ApplyInfix(${render(left)}, $op, ${render(right)})")

  //       case Literal(value) =>
  //         b.append(s"Literal($value)")

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
