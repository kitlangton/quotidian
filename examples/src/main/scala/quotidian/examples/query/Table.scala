package quotidian.examples.query
import quotidian.*
import quotidian.syntax.*
import scala.quoted.*

trait Record extends Selectable:
  inline def selectDynamic(name: String): Any = ${ Record.selectDynamicImpl[this.type]('name) }

object Record:
  def selectDynamicImpl[A: Type](name: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val tpe     = TypeRepr.of[A]
    val nameStr = name.valueOrAbort

    // val fields = tpe.widenTermRefByName match
    //   case Refinement(parent, name, tpe) =>
    //     report.errorAndAbort(s"show all: ${parent.show}\n- ${name}\n ${tpe.show}")
    def fieldsFromRefinement(tpe: TypeRepr): List[(String, TypeRepr)] = tpe match
      case Refinement(parent, name, tpe) =>
        (name, tpe) :: fieldsFromRefinement(parent)
      case _ => Nil

    val fields = fieldsFromRefinement(tpe.widenTermRefByName).toMap

    val field = fields(nameStr)
    field.widenTermRefByName.asType match
      case '[java.lang.String] =>
        '{ "DEFAULT" }
      case '[Int] =>
        '{ 0 }
      case '[other] =>
        '{ () }
      // report.errorAndAbort(s"Unsupported type: ${Type.show[other]}")

trait Query[A]:
  def run: A = (new Record {}).asInstanceOf[A]

  transparent inline def join[T <: Table](table: T): Query[?] = ${ joinImpl[A, table.Fields] }

object Post extends Table:
  trait Fields:
    def title: String
    def content: String
    def author: String

object User extends Table:
  trait Fields:
    def name: String
    def age: Int

  def posts: Post.type = Post

trait Table:
  type Fields

  trait Field[Name <: String, A]

  transparent inline def select(inline ctx: Fields => Any*): Query[?] =
    ${ selectImpl('ctx) }

def selectImpl[Fields](ctx: Expr[Seq[Fields => Any]])(using Quotes): Expr[Query[?]] =
  import quotes.reflect.*

  ctx match
    case Varargs(c) =>
      val namesAndTypes = c.map { f =>
        f.asTerm match
          case Lambda(_, select @ Select(_, name)) =>
            name -> select.tpe.widenTermRefByName
      }
      val refinement = Refinement.of[Record](namesAndTypes)

      refinement.asType match
        case '[t] =>
          '{ new Query[t] {} }

def joinImpl[A: Type, T <: Table#Fields: Type](using Quotes): Expr[Query[?]] =
  import quotes.reflect.*
  val fields = TypeRepr.of[T].widenTermRefByName.typeSymbol.declaredMethods
  val tpes = fields.map { f =>
    f.name -> f.returnType.widenTermRefByName
  }

  val refinement  = Refinement.of[Record](tpes)
  val name        = TypeRepr.of[T].typeSymbol.owner.name.dropRight(1).toLowerCase() + "s"
  val refinement2 = Refinement(TypeRepr.of[A], name, refinement)

  // report.errorAndAbort(s"hey: ${refinement2.show}")
  refinement2.asType match
    case '[t] =>
      '{ new Query[t] {} }
