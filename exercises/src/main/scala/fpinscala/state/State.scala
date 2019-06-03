package fpinscala.state

import State._

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt // ?? What does _ mean in this syntax?

  def boolean: Rand[Boolean] = {
    rng =>
      val (ni, rng2) = rng.nextInt
      (ni < 0, rng2)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (ni, rng2) = rng.nextInt
    // (math.abs(ni), rng2) skewed
    (if (ni < 0) -ni + 1 else ni, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    val j = i / Int.MaxValue
    (j, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (j, rng3) = double(rng2)
    ((i,j), rng3)
  }


  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (j, rng3) = double(rng2)
    ((j,i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  //recursive
  // Why does rng go up by 2??
  // Does this therefore have a bug, where we wind up using `r2` "by accident" in the next (recursive) call to `ints`?
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count - 1)(r1)
      (i :: l, r2)

    }

  // tail-recursive
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(remaining: Int, xs: List[Int], r: RNG) : (List[Int], RNG) = {
      if (remaining == 0) {
        (List(), r)
      }
      else {
        val (i, r2) = rng.nextInt
        go(remaining - 1, i :: xs, r2)
      }
    }
    go(count, List(), rng)
  }

  // Unpacking values (but a pattern match doesn't make sense here)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  // Challenge: implement with foldRight
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      def go(acc : List[A], fs : List[Rand[A]], r : RNG) : (List[A], RNG) = fs match {
        case h :: t =>
          val (hh, r2) = h(r)
          go(hh :: acc, t, r2)
        case Nil => (acc, r)
      }

      go(List(), fs, rng)
    }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    // Destructured assignments require 2 lines of code
    // 2 lines of code require being wrapped in braces
    rng => {
      val (a, r2) = f(rng)
      g(a)(r2)
    }

  // Use flatMap
  // Recursive?
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (ni, rng2) = rng.nextInt
//      flatMap(nonNegativeInt(rng))(rng)
      ???
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State((s : S) => {
      val (a, ss) = this.run(s)
      (f(a), ss)
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = this.run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: => A): State[S, A] = {
    // Why is (s: S) in parentheses? Just for defining the anonymous function?
    State((s : S) => (a, s))
  }

  // Copy-pasted from answers-- is this different from the RNG one?
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }



  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
