package state

import state.RNG.Simple

import scala.collection.mutable.ListBuffer

/**
  * p99 list 6-2
  */
trait RNG {
  def nextInt: (Int, RNG)
}



object RNG {

  /**
    * p100 list6-3
    *
    * @param seed
    */
  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }



  def nonNegativeInt(rng:RNG):(Int, RNG) = {
    val (n, next) = rng.nextInt
    if (n < 0) {
      (-n, next)
    } else if (n == Int.MinValue) {
      (Int.MaxValue, next)
    } else {
      (n, next)
    }
  }

  def double(rng:RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), next)
  }

  val double2:Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng:RNG): ((Int,Double), RNG) = {
    val (n, next) = rng.nextInt
    ((n, double(rng)._1), next)
  }


  def doubleInt(rng:RNG) : ((Double, Int), RNG) = {
    val (id, next) = intDouble(rng)
    ((id._2, id._1), next)
  }

  def double3(rng:RNG) : ((Double, Double, Double),RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng2)
  }

  def ints(count:Int)(rng:RNG):(List[Int], RNG) = {
    if (count > 0) {
      val (list, rng1) = ints(count - 1)(rng)
      val (n, rng2) = rng1.nextInt
      ((list :+ n), rng2)
    } else {
      (List(), rng)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a:A): Rand[A] = rng => (a, rng)

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Excercise 6.7の答え
  def sequence[A](fs:List[Rand[A]]) = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)((a, b) => (a :: b)))
  }
  // わかりにくいので型をつけた
  def sequence_verbose[A](fs:List[Rand[A]]):Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f:Rand[A], acc:Rand[List[A]]) => map2(f, acc)((a:A, b:List[A]) => (a :: b)))
  }

  def nonNegativeLessThan2(n:Int):Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]) : Rand[B] = rng => {
    val (a, rng2) = f(rng)
    val rb:Rand[B] = g(a)
    rb(rng2)
  }

  def map_old[A, B](s: Rand[A])(f:A => B):Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2_old[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A, B) => C):Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def map[A, B](s: Rand[A])(f:A => B):Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A, B) => C):Rand[C] = flatMap(ra) {
    a => flatMap(rb) {
      b => unit(f(a, b))
    }
  }


  def nonNegativeLessThan(n:Int):Rand[Int] = {
    flatMap(nonNegativeInt) { i:Int =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }
}



object Test {
  def main_old(args:Array[String]): Unit = {
    val s1 = new RNG.Simple(10)
    val (n1, s2) = s1.nextInt
    println(n1)
    val (n2, s3) = RNG.nonNegativeInt(s2)
    println(n2)
    val (d3, s4) = RNG.double(s3)
    println(d3)
    val (d4, s5) = RNG.double(s3)
    println(d4)
    val (d5, s6) = RNG.intDouble(s4)
    println(d5)
    val (list, s7) = RNG.ints(10)(s4)
    println(list)
  }

  def main(args:Array[String]) = {
    val s1 = Simple(10)
    val d1 = RNG.double(s1)
    println(d1)
    println(RNG.double2(s1))

  }

  def main_old1(args:Array[String]) = {
    val s = Simple(10)
    val nl10 = RNG.nonNegativeLessThan(10)(s)
    println(nl10)
  }
}
