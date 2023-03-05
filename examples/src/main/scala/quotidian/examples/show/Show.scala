package quotidian.examples.show
import Show.show

trait Show[A]:
  def show(a: A): String

object Show:
  extension [A: Show](a: A) def show: String = summon[Show[A]].show(a)

  given Show[Int] with
    def show(a: Int): String = a.toString

  given Show[String] with
    def show(a: String): String = a

  given [A: Show]: Show[Option[A]] with
    def show(a: Option[A]): String = a match
      case Some(a) => "Some(" + a.show + ")"
      case None    => "None"

  inline def derived[A]: Show[A] = ${ ShowMacro.deriveShow[A] }
