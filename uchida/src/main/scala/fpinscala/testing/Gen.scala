package fpinscala.testing

import fpinscala.state.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}

object Gen {

  //  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  //    RNG.Simple(1)
  //    val state: State[RNG, Int] = State.unit(1)
  //    Gen(state)
  //  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}

object Test {

  def main(args: Array[String]): Unit = {

    val gen: Gen[List[Int]] = Gen.listOfN(10, Gen.unit(2))
    println("ランダムな配列")
    println(gen.sample.run(RNG.Simple(10000L))._1)
  }

}
