package quotidian

import quotidian.DeriveFromExprSpecMacro.{testRoundTripFruit, testRoundTripJob, testRoundTripPerson}

import scala.quoted.*
import zio.test.*

object DeriveFromExprSpec extends ZIOSpecDefault:
  val spec = suite("DeriveFromExprSpec")(
    test("Product Derivation") {
      inline def fido   = Pet("Fido", true, None)
      inline def person = Person("John", 40, fido)
      val person2       = testRoundTripPerson(person)
      assertTrue(person == person2)
    },
    test("Enum Derivation") {
      inline def fruit: Fruit = Fruit.Apple("Granny Smith")
      val fruit2              = testRoundTripFruit(fruit)
      assertTrue(fruit == fruit2)
    },
    test("Sealed Trait Derivation") {
      inline def job: Job = Job.Manager("Billie The Manager", true)
      val job2            = testRoundTripJob(job)
      assertTrue(job == job2)
    }
  )
