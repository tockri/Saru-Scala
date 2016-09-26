package fpinscala.testing

import fpinscala.state.RNG.Rand
import fpinscala.state.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {

//  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
//    RNG.Simple(1)
//    val state: State[RNG, Int] = State.unit(1)
//    Gen(state)
//  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}
