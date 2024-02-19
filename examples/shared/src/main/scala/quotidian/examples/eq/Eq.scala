package quotidian.examples.eq

trait Eq[A]:
  def eqv(x: A, y: A): Boolean

object Eq:
  extension [T](x: T) def ===(y: T)(using eq: Eq[T]): Boolean = eq.eqv(x, y)

  given Eq[String] with
    def eqv(x: String, y: String) = x == y

  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  given Eq[Boolean] with
    def eqv(x: Boolean, y: Boolean) = x == y

  given Eq[Double] with
    def eqv(x: Double, y: Double) = x == y

  given [A: Eq]: Eq[Option[A]] with
    def eqv(x: Option[A], y: Option[A]) = (x, y) match
      case (Some(x), Some(y)) => x === y
      case (None, None)       => true
      case _                  => false

  inline def derived[A] = ${ EqMacros.deriveEq[A] }
