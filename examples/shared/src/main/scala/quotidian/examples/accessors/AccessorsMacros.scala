package quotidian.examples.accessors

import quotidian.*
import quotidian.syntax.*

import quoted.*
import quotidian.examples.lens.Lens.make
import zio.*
import java.lang.reflect.Method

object DeriveAccessors:

  def delegate[Service: Type, R: Type, E: Type, A: Type](using
      Quotes
  )(serviceExpr: Expr[Service], symbol: quotes.reflect.Symbol, args: List[quotes.reflect.Term]): Expr[ZIO[R, E, A]] =
    import quotes.reflect.*
    serviceExpr.asTerm.call(symbol, args).asExprOf[ZIO[R, E, A]]

  def makeAccessorImpl[Service: Type](using Quotes)(symbol: quotes.reflect.Symbol): Expr[Any] =
    import quotes.reflect.*

    symbol.returnType.asType match
      case '[ZIO[r, e, a]] =>
        val methodType = symbol.termRef.widenTermRefByName.asInstanceOf[MethodType]

        val newMethodType = methodType match
          case MethodType(args, targs, result) =>
            MethodType.apply(args)(_ => targs, _ => TypeRepr.of[ZIO[Service, e, a]])

        def call(serviceExpr: Expr[Service], args: List[Term]): Expr[ZIO[r, e, a]] =
          serviceExpr.asTerm.call(symbol, args).asExprOf[ZIO[r, e, a]]

        def innerLambda(args: List[Term]): Expr[Service => ZIO[r, e, a]] =
          '{ (service: Service) => ${ call('service, args) } }

        Lambda
          .apply(
            Symbol.spliceOwner,
            newMethodType,
            (methSym, args) =>
              given Quotes = methSym.asQuotes
              '{
                ZIO.serviceWithZIO[Service] {
                  ${ innerLambda(args.asInstanceOf[List[Term]]) }
                }
              }.asTerm
          )
          .asExpr

  implicit transparent inline def gen[S]: Any = ${ makeAccessorsImpl[S] }

  def makeAccessorsImpl[S: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    val methods = TypeRepr.of[S].typeSymbol.declaredMethods

    val accessorImpls = methods.map { method =>
      method.name -> makeAccessorImpl[S](method)
    }

    val refinedType =
      Refinement.of[AccessorSelector[S]](
        accessorImpls.map { case (name, impl) =>
          name -> impl.asTerm.tpe.widenTermRefByName
        }
      )

    val accessorMap = Expr.ofMap(accessorImpls)

    refinedType.asType match
      case '[t] =>
        '{ new AccessorSelector[S]($accessorMap).asInstanceOf[t] }

private trait AccessorsFor[A]:
  type Out
  def Accessors: Out

private object AccessorsFor:
  transparent inline given derived[A]: AccessorsFor[A] = ${ AccessorsForImpl[A] }

  def AccessorsForImpl[A: Type](using Quotes) =
    import quotes.reflect.*
    val AccessorsExpr = DeriveAccessors.makeAccessorsImpl[A]
    AccessorsExpr.asTerm.tpe.asType match
      case '[t] =>
        '{ AccessorsFor.make[A, t]($AccessorsExpr.asInstanceOf[t]) }

  private def make[A, Accessors](Accessors0: Accessors): AccessorsFor[A] { type Out = Accessors } =
    new AccessorsFor[A]:
      type Out = Accessors
      def Accessors: Accessors = Accessors0

trait DeriveAccessors[A]:
  type Self = this.type

  transparent inline given conversion(using Accessors: AccessorsFor[A]): Conversion[Self, Accessors.Out] =
    new Conversion[Self, Accessors.Out]:
      def apply(a: Self): Accessors.Out = Accessors.Accessors
