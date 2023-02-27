package quotidian

import scala.quoted.*

final case class Person(name: String, age: Int, pets: Pet)

object Person:

  given FromExpr[Person] = DeriveFromExpr.derived

final case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person])

object Pet:
  given FromExpr[Pet] = DeriveFromExpr.derived

enum Fruit:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)

object Fruit:
  given FromExpr[Fruit] = DeriveFromExpr.derived

sealed trait Job extends Product with Serializable

object Job:
  case class Developer(name: String, tickets: Int)  extends Job
  case class Manager(name: String, isBusy: Boolean) extends Job
  case class HolePuncher(cognomen: String)          extends Job

  given FromExpr[Job] = DeriveFromExpr.derived
