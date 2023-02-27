package quotidian

import scala.quoted.*

final case class Person(name: String, age: Int, pets: Pet) derives FromExpr, ToExpr
final case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person]) derives FromExpr, ToExpr

enum Fruit derives FromExpr, ToExpr:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)

sealed trait Job extends Product with Serializable derives FromExpr, ToExpr

object Job:
  case class Developer(name: String, tickets: Int)  extends Job
  case class Manager(name: String, isBusy: Boolean) extends Job
  case class HolePuncher(cognomen: String)          extends Job
  case class CouchPotato()                          extends Job
  case object Lump                                  extends Job
