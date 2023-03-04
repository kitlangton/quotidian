package quotidian.examples.show

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

  inline def derived[A]: Show[A] = ${ ShowMacro.showImpl[A] }

import Show.*

final case class Person(name: String, age: Int) derives Show
final case class Pet(name: String, bones: Int, owner: Option[Person]) derives Show

enum Color derives Show:
  case Red
  case Green(name: String)
  case Blue

object ShowDemo extends App:
  val person = Person("Alice", 40)
  val pet    = Pet("Fido", 3, Some(person))
  println(pet.show)
  val color: Color = Color.Green("Emerald")
  println(color.show)
