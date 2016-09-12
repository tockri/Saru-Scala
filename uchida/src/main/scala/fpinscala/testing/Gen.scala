package fpinscala.testing

import fpinscala.state.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}
