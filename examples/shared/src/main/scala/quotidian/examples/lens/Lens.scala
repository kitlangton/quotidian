package quotidian.examples.lens

trait Lens[S, A]:
  def get(s: S): A
  def set(s: S, a: A): S

  def modify(s: S)(f: A => A): S =
    set(s, f(get(s)))

object Lens:
  inline def make[S]: Make[S] = new Make[S]

  class Make[S]:
    inline def apply[A](inline selector: S => A): Lens[S, A] =
      ${ LensMacros.makeLensImpl[S, A]('selector) }
