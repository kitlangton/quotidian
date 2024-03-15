package quotidian.examples.debug

import quotidian.*

enum DebugLevel:
  case Debug, Info, Warn, Error

  inline def nice: Int = 12

object Main extends App:
  println("Hello, world!")
  // Debug.debug(DebugLevel.Debug)
