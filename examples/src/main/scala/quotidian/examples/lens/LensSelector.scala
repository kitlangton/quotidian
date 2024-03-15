package quotidian.examples.lens

class LensSelector[A](lenses: Map[String, Lens[A, ?]]) extends Selectable:
  inline def selectDynamic(name: String): Lens[A, ?] =
    lenses(name)
