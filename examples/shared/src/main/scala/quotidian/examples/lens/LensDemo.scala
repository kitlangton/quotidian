package quotidian.examples.lens

import quotidian.examples.lens.Person.age

case class Person(name: String, age: Int)

object Person:
  val lenses = LensMacros.makeLenses[Person]
  val age    = lenses.age
  val name   = lenses.name

@main
def example(): Unit =
  val person = Person("Alice", 42)
  println(s"Person: $person")

  val name = Person.name.get(person)
  println(s"Name: $name")

  val aged = Person.age.modify(person)(_ + 100)
  println(s"Aged: $aged")
