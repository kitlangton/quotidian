package quotidian.examples.show

import Show.*

import scala.deriving.Mirror

final case class Person(name: String, age: Int) derives Show
final case class Pet(name: String, bones: Int, owner: Option[Person]) derives Show

enum Color derives Show:
  case Red
  case Green(name: String)
  case Blue

object ShowDemo extends App:
  summon[Mirror.Of[Color.Red.type]]
  val person = Person("Alice", 40)
  val pet    = Pet("Fido", 3, Some(person))
  println(pet.show)
  val color: Color = Color.Green("Emerald")
  println(color.show)
