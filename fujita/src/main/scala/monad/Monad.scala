package monad

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.parsing._
import fpinscala.errorhandling._


object Study {
  def main(args: Array[String]): Unit = {
    val l = Monad.listMonad
    // println(l.replicateM2(4, List(1, 2, 3)))

    // println(l.filterM(List(1, 2, 3, 4, 5))(a => List(a % 2 == 0)))

    val f:Int => List[Int] = a => List(a + 1)
    val g:Int => List[Int] = a => List(a * 2)
    val h:Int => List[Int] = a => List(a / 3)
    println(l.compose(l.compose(f, g), h)(5))
    println(l.compose(f, l.compose(g, h))(5))
  }
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List())
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def replicateM2[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List())
    else {
      flatMap(ma)(a => flatMap(replicateM2(n - 1, ma))(b => unit(a :: b)))
    }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {a =>
    flatMap(f(a))(g)
  }

}

object Monad {

  val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

}
