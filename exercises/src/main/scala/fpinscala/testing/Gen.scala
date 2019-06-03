package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// Exercise 8.3
//trait Prop {
//  def check : Boolean
//  def &&(p: Prop): Prop = new Prop {
//    def check = this.check && p.check
//  }
//}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]


}


object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]) : Gen[B] = {
    //Gen(sample.flatMap(a => f(a).sample))
    val (a, rng2) = this.sample.run
    f(a)
  }

  def listOfN(n: Int) : Gen[List[A]] = {
    // Using awesome helper methods from previous units
    Gen(State.sequence(List.fill(n)(this.sample)))
  }

  def listOfGeN(size: Gen[Int]) : Gen[List[A]] = {
    size.flatMap(n => this.listOfN(n))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] = {
    // flatMap the sample
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, ga: Gen[A]) : Gen[List[A]] = {
    // Using awesome helper methods from previous units
    Gen(State.sequence(List.fill(n)(ga.sample)))
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) : Gen[A] = {
    val threshold = g1._2 / (g1._2 + g2._2)
    // flatMap the sample
    Gen(State(RNG.double).flatMap(s => if (s < threshold) g1._1.sample else g2._1.sample))
  }


}

trait Gen[A] {

}

trait SGen[+A] {

}

