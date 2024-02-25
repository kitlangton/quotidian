package quotidian.examples.lens

import quotidian.examples.lens.Person.age
import scala.annotation.experimental

case class Person(name: String, age: Int):
  def randomNumber = 42

object Person:
  val lenses = LensMacros.makeLenses[Person]
  val age    = lenses.age
  val name   = lenses.name

@experimental
@main
def example(): Unit =
  val person = Person("Alice", 42)
  val name   = Person.name.get(person)
  val aged   = Person.age.set(person, 43)
  println(s"Person: $person")
  println(s"Name: $name")
  println(s"Aged: $aged")
