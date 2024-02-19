package quotidian.examples.eq

import Eq.*

import scala.deriving.Mirror

final case class Person(name: String, age: Int) derives Eq
final case class Pet(name: String, bones: Int, owner: Option[Person]) derives Eq

enum Color derives Eq:
  case Red(intensity: Int)
  case Green(name: String)
  case Blue(vibration: Double, isMajestic: Boolean)
//
object EqDemo extends App:
  val p1 = Person("Alice", 43)
  val p2 = Person("Bob", 42)
  val p3 = Person("Alice", 41)

  println(s"${p1} === ${p2}: ${p1 === p2}")
  println(s"${p1} === ${p3}: ${p1 === p3}")

  val c1 = Color.Red(42)
  val c2 = Color.Green("Emerald")
  val c3 = Color.Blue(42.0, true)
  val c4 = Color.Blue(43.0, true)

  println(s"${c1} === ${c1}: ${c1 === c1}")
  println(s"${c1} === ${c2}: ${c1 === c2}")
  println(s"${c3} === ${c4}: ${c3 === c4}")
