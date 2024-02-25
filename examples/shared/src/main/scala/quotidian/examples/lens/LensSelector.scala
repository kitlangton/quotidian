package quotidian.examples.lens

import quotidian.examples.lens.Person.age
import scala.annotation.experimental

class LensSelector[A](lenses: Map[String, Lens[A, ?]]) extends Selectable:
  inline def selectDynamic(name: String): Lens[A, ?] =
    lenses(name)
