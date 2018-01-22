package fpinscala.exercise

import saruscala.datastructurs.{List, Cons, Nil}

object Section3 {

  /**
    * リスト3-3
    */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def mkstring[A](as:List[A]):String = foldLeft(as, "")((b, a) => b + ", " + a.toString)

  def exercise3_10(): Unit = {
    println("sum:" + foldLeft(List(1, 2, 3), 0)((b, a) => a + b))
  }

  def sum(as: List[Int]) = foldLeft(as, 0)((b, a) => a + b)

  def product(as: List[Double]) = foldLeft(as, 1.0)((b, a) => a * b)

  def length[A](as: List[A]):Int = foldLeft(as, 0)((b, a) => b + 1)

  def exercise3_11(): Unit = {
    println("sum:" + sum(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    println("product:" + product(List(1.0, 2.0, 3.0)))
    println("length:" + length(List(1, 2, 3, 4)))
  }

  def reverse[A](as: List[A]) = foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

  def exercise3_12():Unit = {
    println("reverse: " + mkstring(reverse(List(1, 2, 3))))
  }

  def exercise3_5(): Unit = {
    val l = List(1, 2, 3)
    println(List.dropWhile[Int](l, a => a < 3))
  }

  def exercise3_6(): Unit = {
    val l = List(1, 2, 3, 4)
    println("answer:")
    println(List.init(l))
  }

  def main(args: Array[String]): Unit = {
    exercise3_12()
  }
}

