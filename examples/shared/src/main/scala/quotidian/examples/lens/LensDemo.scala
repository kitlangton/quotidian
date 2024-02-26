package quotidian.examples.lens

case class Person(name: String, age: Int, isAlive: Boolean)
object Person extends DeriveLenses[Person]

@main
def example(): Unit =
  val person  = Person("Alice", 42, true)
  val name    = Person.name.get(person)
  val age     = Person.age.get(person)
  val isAlive = Person.isAlive.get(person)

  println(s"Name: $name, Age: $age, Is Alive: $isAlive")
