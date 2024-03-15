package quotidian.examples.accessors

import zio.*
import scala.concurrent.duration.fromNow

trait ExampleService:
  def launchRockets(): Task[Unit]
  def addNumbers(a: Int, b: Int): UIO[Int]

object ExampleService extends DeriveAccessors[ExampleService]

object Example extends ZIOAppDefault:
  val program: ZIO[ExampleService, Throwable, Unit] =
    for
      _ <- ExampleService.addNumbers(1, 2)
      _ <- ExampleService.launchRockets()
    yield ()

  val run =
    program.provide(ExampleServiceLive.layer)

// -

case class ExampleServiceLive() extends ExampleService:

  def launchRockets(): Task[Unit] = ZIO.debug("Launching rockets!")
  def addNumbers(a: Int, b: Int)  = ZIO.succeed(a + b).debug(s"$a + $b = ")

object ExampleServiceLive:
  val layer = ZLayer.succeed(ExampleServiceLive())
