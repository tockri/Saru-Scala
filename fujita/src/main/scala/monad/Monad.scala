package monad

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.parsing._
import fpinscala.errorhandling._


object Study {
  def main(args: Array[String]): Unit = {
    println("Saru")
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
