//package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // recursive
  // Cons, the class, used in pattern matching
  // Cons(h, t) has thunks h and t, h: => A, t: => Stream[A]
  // So h() : A, and t() : Stream[A]
  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => List()
    }
  }

  // tail-recursive
  def toListTR: List[A] = {
    @annotation.tailrec
    def go(acc: List[A], str: Stream[A]): List[A] = {
      str match {
        case Empty => acc
        case Cons(h, t) => go(h() :: acc, t())
      }
    }

    go(List(), this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t() take n - 1)
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }


  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => Empty
  }

  // Use foldRight to implement these:
  final def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)


  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) =>
      if (p(h())) t().forAll(p)
      else false
    case _ => true
  }

  // Make use of lazy evaluation of &&
  def forAllFR(p: A => Boolean): Boolean = {
    foldRight(true)((x, y) => p(x) && y)
  }


  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def tailOption: Option[Stream[A]] = this match {
    case Cons(h, t) => Some(t())
    case _ => None
  }

  def headOptionFR: Option[A] = {
    foldRight(None: Option[A])((x, y) => Some(x)) // Don't evaluate y
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def mapU[B](f: A => B) : Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }






}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    val y : Int = 20
    val d = Int.MinValue
    y.abs
    def fibsSum(a: Int, b: Int): Stream[Int] = {
      cons(a + b, fibsSum(b, a + b))
    }
    cons(0, cons(1, fibsSum(0, 1)))
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def onesU() : Stream[Int] =
    unfold(1) {
      x: Int => Some((1, 1))
    }

  def fromU(n : Int) : Stream[Int] =
    unfold(n) {x => Some((x, x+1))}

  def constantU(n : Int) : Stream[Int] =
    unfold(n) {x => Some((x, x))}

  val foo = (0,1)

  def fibsU() : Stream[Int] =
    unfold((0,1)) {case (a, b) => Some((a, (b, a + b)))}

  def fibsU2() : Stream[Int] =
    unfold((0,1)) {t : (Int, Int) => Some((t._1, (t._2, t._1 + t._2)))}


}

object Cons {
  def main(args: Array[String]) {
    println(fibsU2().takeWhile( p => p < 30).toString)
  }
}