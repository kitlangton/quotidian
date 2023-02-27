package quotidian

import quotidian.DeriveFromExprSpecMacro.{testFromExprFruit, testFromExprJob, testFromExprPerson}

import scala.quoted.*
import zio.test.*

object DeriveFromExprSpec extends ZIOSpecDefault:
  val spec = suite("DeriveFromExprSpec")(
    test("Product Derivation") {
      inline def fido   = Pet("Fido", true, None)
      inline def person = Person("John", 40, fido)
      testFromExprPerson(person)
      assertCompletes
    },
    test("Enum Derivation") {
      inline def fruit: Fruit = Fruit.Apple("Granny Smith")
      testFromExprFruit(fruit)
      assertCompletes
    },
    test("Sealed Trait Derivation") {
      inline def job: Job = Job.Manager("Billie The Manager", true)
      testFromExprJob(job)
      assertCompletes
    }
  )
