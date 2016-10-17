package property

import fpinscala.state.{State, RNG}


/**
  * 8章 プロパティベースのテスト
  */
object Study {

  def main(args:Array[String]) = {
    val c = (new TestProp && new TestProp).check
    println(c)
    println("-----------")
    val intGen:Gen[Int] = Gen(State {rng =>rng.nextInt})
    val gen:Gen[List[Int]] = Gen.listOfN(10, intGen)
    println("ランダムな配列")
    println(gen.sample.run(RNG.Simple(10000L))._1)
    // val gen:Gen[List[Int]] = Gen.listOfN(10, Gen.choose(1,100))
    // println("ランダムな配列")
    // println(gen.sample.run(RNG.Simple(10000L))._1)
    println("-----------")
  }

  def sum(ints:List[Int]):Int = {
    ints.foldLeft(0)((s, elem) => elem + s)
  }
}

class TestProp extends Prop {
  def check:Boolean = {
    println("checking TestProp!!")
    true
  }
}


/**
  * テスト用値のジェネレータ
  * @param sample
  * @tparam A
  */
case class Gen[A](sample:State[RNG, A])

object Gen {

  def unit[A](a: => A):Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]):Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


  def choose(start:Int, stopExclusive:Int):Gen[Int] = {
    val state:State[RNG,Int] = State(RNG.nonNegativeInt)
    Gen(state.map(n => start + n % (stopExclusive-start)))
  }
}

trait Prop {
  def check:Boolean

  def &&(p:Prop):Prop = new Prop {
    def check = p.check && Prop.this.check
  }
}