import MacroTestHelpers.*
import SyntaxSpecMacro.ExampleTraitLive.nullary
import quotidian.syntax.*

import scala.Console
import scala.quoted.*

object SyntaxSpecMacro:

  inline def spec: Unit = ${ specImpl }

  trait ExampleTrait:
    def nullary: Int
    def nullary2(): Int

    def add(x: Int, y: Int): Int
    def repeatString(s: String, n: Int): String
    def maybeDivide(x: Int, y: Int): Option[Int]
    def eitherDivide(x: Int, y: Int): Either[String, Int]
    def makeListOfOption(x: Int): List[Option[Int]]

    def withTypeArg[A]: Int                          = 1
    def withTypeArg2[A](): Int                       = 1
    def identity[A](a: A): A                         = a
    def makeEither[E, A](a: A)(b: Int): Either[E, A] = Right(a)

    def multipleParamLists(x: Int)(y: Int): Int = x + y

  object ExampleTraitLive extends ExampleTrait:
    def nullary: Int                  = 12
    def nullary2(): Int               = 13
    override def identity[A](a: A): A = a

    def add(x: Int, y: Int): Int                 = x + y
    def repeatString(s: String, n: Int): String  = s * n
    def maybeDivide(x: Int, y: Int): Option[Int] = if y == 0 then None else Some(x / y)
    def eitherDivide(x: Int, y: Int): Either[String, Int] =
      if y == 0 then Left("Cannot divide by zero") else Right(x / y)

    def makeListOfOption(x: Int): List[Option[Int]] = List(Some(x), None, Some(x * 2))

  def specImpl(using ctx: Quotes): Expr[Unit] =
    import ctx.reflect.*
    testMethodInfo
    testMakeTuple
    testFinalReturnType
    testParamTypes
    testTermCall
    '{ () }

  def testMethodInfo(using ctx: Quotes): Unit =
    import ctx.reflect.*
    val typeRepr   = TypeRepr.of[ExampleTrait]
    val methods    = typeRepr.typeSymbol.declaredMethods
    val method     = methods.find(_.name == "makeEither").get
    val methodType = method.termRef.widenTermRefByName
    println(methodType)
    println(s"isDefDef ${method.isDefDef}")
    methodType match
      // case DefDef(_, _, _, _) =>
      //   println(s"I am a DefDef")
      case PolyType(typeParams, tpes, MethodType(labels, otherTypes, result)) =>
        println(s"""
        | typeParams: ${typeParams}
        | tpesLength: ${tpes.length}
        | tpes: ${tpes}}
        | labels: ${labels}
        | otherTypes: ${otherTypes}
        | result: ${result}
        """.stripMargin)
    println(method.paramSymss)

  def testTermCall(using ctx: Quotes): Unit =
    import ctx.reflect.*
    val typeRepr       = TypeRepr.of[ExampleTrait]
    val methods        = typeRepr.typeSymbol.declaredMethods
    val instance: Term = '{ ExampleTraitLive }.asTerm

    val add       = methods.find(_.name == "add").get
    val addResult = instance.call(add, List(Literal(IntConstant(1)), Literal(IntConstant(2))))
    println(s"result: ${addResult.show}")
    addResult.asExprOf[Int]

    val nullary       = methods.find(_.name == "nullary").get
    val nullaryResult = instance.call(nullary)
    println(s"result: ${nullaryResult.show}")
    println(s"result: ${nullaryResult}")
    nullaryResult.asExprOf[Int]

    val nullary2       = methods.find(_.name == "nullary2").get
    val nullary2Result = instance.call(nullary2)
    println(s"result: ${nullary2Result.show}")
    println(s"result: ${nullary2Result}")
    nullary2Result.asExprOf[Int]

    val repeatString = methods.find(_.name == "repeatString").get
    val repeatStringResult =
      instance.call(repeatString, List(Literal(StringConstant("hello")), Literal(IntConstant(3))))
    println(s"result: ${repeatStringResult.show}")
    repeatStringResult.asExprOf[String]

    val identity       = methods.find(_.name == "identity").get
    val identityResult = instance.call(identity, List(Literal(IntConstant(42))))
    println(s"result: ${identityResult.show}")

    identityResult.asExprOf[Int]

    val withTypeArg       = methods.find(_.name == "withTypeArg").get
    val withTypeArgResult = instance.call(withTypeArg, List(TypeRepr.of[Int]), List.empty[Term])
    println(s"result: ${withTypeArgResult.show}")
    withTypeArgResult.asExprOf[Int]

    val withTypeArg2       = methods.find(_.name == "withTypeArg2").get
    val withTypeArg2Result = instance.callType(withTypeArg2, List(TypeRepr.of[Int]))
    println(s"result: ${withTypeArg2Result.show}")

    val multipleParamLists = methods.find(_.name == "multipleParamLists").get
    val multipleParamListsResult = instance
      .call(
        multipleParamLists,
        List(Literal(IntConstant(1)))
      )
      .appliedTo(Literal(IntConstant(2)))
    println(s"result: ${multipleParamListsResult.show}")
    multipleParamListsResult.asExprOf[Int]

    val multipleParamListsCurriedResult = instance.call(
      multipleParamLists,
      List(Literal(IntConstant(1)))
    )
    println(s"result: ${multipleParamListsCurriedResult.show}")
    multipleParamListsCurriedResult.etaExpand(Symbol.noSymbol).asExprOf[Int => Int]

  def testMakeTuple(using ctx: Quotes): Unit =
    import ctx.reflect.*
    assertEqualTypes(
      TypeRepr.makeTuple(List(TypeRepr.of[Int], TypeRepr.of[String])),
      TypeRepr.of[(Int, String)],
      "makeTuple(Int, String) is correct"
    )

    val tupleClass = TypeRepr.makeTupleClass(List(TypeRepr.of[Int], TypeRepr.of[String]))
    assertEqualTypes(
      tupleClass,
      TypeRepr.of[Int *: String *: EmptyTuple],
      "makeTupleClass(Int, String) is correct"
    )

    assertEqualTypes(
      TypeRepr.makeTuple(List(TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[Boolean])),
      TypeRepr.of[(Int, String, Boolean)],
      "makeTuple(Int, String, Boolean) is correct"
    )

  def testFinalReturnType(using ctx: Quotes): Unit =
    import ctx.reflect.*
    val typeRepr = TypeRepr.of[ExampleTrait]

    val methods = typeRepr.typeSymbol.declaredMethods

    def expectReturnType(methodName: String, expected: TypeRepr): Unit =
      methods.find(_.name == methodName) match
        case Some(method) =>
          val returnType = method.returnType
          assertEqualTypes(returnType, expected, s"Method $methodName returns ${returnType.show}")
        case None =>
          report.errorAndAbort(s"Method $methodName not found")

    expectReturnType("add", TypeRepr.of[Int])
    expectReturnType("repeatString", TypeRepr.of[String])
    expectReturnType("maybeDivide", TypeRepr.of[Option[Int]])
    expectReturnType("eitherDivide", TypeRepr.of[Either[String, Int]])

    expectReturnType("makeListOfOption", TypeRepr.of[List[Option[Int]]])

  def testParamTypes(using Quotes): Unit =
    import quotes.reflect.*
    val typeRepr = TypeRepr.of[ExampleTrait]
    val methods  = typeRepr.typeSymbol.declaredMethods

    def expectParamTypes(methodName: String, expected: List[TypeRepr]): Unit =
      methods.find(_.name == methodName) match
        case Some(method) =>
          val paramTypes = method.paramTypes
          assertEqualTypeLists(paramTypes, expected, s"Method $methodName has param types ${paramTypes.map(_.show)}")
        case None =>
          report.errorAndAbort(s"Method $methodName not found")

    expectParamTypes("add", List(TypeRepr.of[Int], TypeRepr.of[Int]))
    expectParamTypes("repeatString", List(TypeRepr.of[String], TypeRepr.of[Int]))
    expectParamTypes("maybeDivide", List(TypeRepr.of[Int], TypeRepr.of[Int]))
    expectParamTypes("eitherDivide", List(TypeRepr.of[Int], TypeRepr.of[Int]))
    expectParamTypes("makeListOfOption", List(TypeRepr.of[Int]))

// NOTE: Make a full-blown test framework
object MacroTestHelpers:

  def assertEqualTypes(using
      Quotes
  )(lhs: quotes.reflect.TypeRepr, rhs: quotes.reflect.TypeRepr, message: String): Unit =
    import quotes.reflect.*
    if lhs =:= rhs then println(green(s"- $message"))
    else
      val errorMessage = s"${lhs.show} != ${rhs.show}"
      report.errorAndAbort(red(s"- $message\n  - $errorMessage"))

  def assertEqualTypeLists(using
      ctx: Quotes
  )(lhs: List[ctx.reflect.TypeRepr], rhs: List[ctx.reflect.TypeRepr], message: String): Unit =
    val allSame = lhs.zip(rhs).forall { case (l, r) => l =:= r }
    if allSame then println(green(s"- $message"))
    else
      val errorMessage = s"${lhs.map(_.show)} != ${rhs.map(_.show)}"
      println(red(s"- $message\n  - $errorMessage"))

  def macroAssert(using Quotes)(expectation: => Boolean, message: String): Unit =
    import quotes.reflect.*
    if expectation then println(green(s"- $message"))
    else report.errorAndAbort(s"- assertion failed: $message")

  private def green(str: String): String = Console.GREEN + str + Console.RESET
  private def red(str: String): String   = Console.RED + str + Console.RESET
