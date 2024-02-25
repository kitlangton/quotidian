package quotidian.examples.lens

case class Person(name: String, age: Int, isAlive: Boolean)

object Person:
  val lens = LensMacros.makeLenses[Person]

@main
def example(): Unit =
  val person = Person("Alice", 42, true)
  println(s"Person: $person")

  val name = Person.lens.name.get(person)
  println(s"Name: $name")

  val aged = Person.lens.age.modify(person)(_ + 100)
  println(s"Aged: $aged")
