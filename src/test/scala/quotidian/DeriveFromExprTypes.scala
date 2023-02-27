package quotidian

import scala.quoted.*

final case class Person(name: String, age: Int, pets: Pet) derives FromExpr
final case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person]) derives FromExpr

enum Fruit derives FromExpr:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)

sealed trait Job extends Product with Serializable derives FromExpr

object Job:
  case class Developer(name: String, tickets: Int)  extends Job
  case class Manager(name: String, isBusy: Boolean) extends Job
  case class HolePuncher(cognomen: String)          extends Job
