package quotidian.examples.accessors

class AccessorSelector[A](lenses: Map[String, Any]) extends Selectable:
  inline def selectDynamic(name: String): Any =
    lenses(name)
