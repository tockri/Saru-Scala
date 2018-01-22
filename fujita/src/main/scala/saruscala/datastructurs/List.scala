package saruscala.datastructurs

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[S](ls: List[S]): List[S] = ls match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[S](h: S, ls: List[S]): List[S] = ls match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  // EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => if (n == 0) tail else drop(tail, n - 1)
  }

  // EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, tail) if (f(a)) => dropWhile(tail, f)
    case _ => l
  }

  // EXERCISE 3.6
  def init[A](l:List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def mkstring[A](l:List[A]):String = l match {
    case Nil => ""
    case Cons(a, Nil) => a.toString
    case Cons(h, t) => h.toString + ", " + mkstring(t)
  }

}