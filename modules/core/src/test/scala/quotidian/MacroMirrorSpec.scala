package quotidian

import quotidian.MacroMirrorSpecMacro.macroMirrorTest
import zio.test.*

import scala.quoted.*

object MacroMirrorSpec extends ZIOSpecDefault:

  val spec = suite("MacroMirrorSpec")(
    suite("ProductMacroMirror#construct")(
      test("construct a case class") {
        val result = MacroMirrorSpecMacro.constructProductTest[Pet]("hello", true, None)
        println(result)
        assertTrue(result == Pet("hello", true, None))
      },
      test("construct a parameterized case class") {
        val result = MacroMirrorSpecMacro.constructProductTest[Box[Int, Boolean]]("IntBox", 12, Some(true))
        assertTrue(result == Box("IntBox", 12, Some(true)))
      },
      test("construct a parameterized case class with type ref arg") {
        def build[A, B](value: A, other: Option[B]) =
          MacroMirrorSpecMacro.constructProductTest[Box[A, B]]("IntBox", value, other)
        val result = build(true, Some(12))
        assertTrue(result == Box("IntBox", true, Some(12)))
      },
      test("construct a singleton type case class") {
        val result = MacroMirrorSpecMacro.constructProductTest[Job.Lump.type]()
        assertTrue(result == Job.Lump)
      },
      test("refuse to derive a mirror for primitive type") {
        val result = MacroMirrorSpecMacro.macroMirrorTest[Int]
        assertTrue(result == "Cannot summon Mirror.Of[scala.Int]")
      }
    )
  )
