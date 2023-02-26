package quotidian

import scala.quoted.FromExpr

final case class Person(name: String, age: Int, pets: Pet)

object Person:

  given FromExpr[Person] = DeriveFromExpr.derived

final case class Pet(name: String, hasBone: Boolean)

object Pet:
  given FromExpr[Pet] = DeriveFromExpr.derived
