package quotidian

import quotidian.DeriveFromExprSpecMacro.testFromExprPerson

import scala.quoted.*
import zio.test.*

object DeriveFromExprSpec extends ZIOSpecDefault:
  val spec = suite("DeriveFromExprSpec")(
    test("derive from expr") {
      inline def fido   = Pet("Fido", hasBone = true)
      inline def person = Person("John", 40, fido)
      testFromExprPerson(person)
      assertCompletes
    }
  )
