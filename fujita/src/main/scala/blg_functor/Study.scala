package blg_functor
import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

/**
  * Functorの学習だよー
  */
object Study {
  implicit object OptionFunctor extends Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  def identityLaw[F[_], A](fa: F[A])(implicit F: Functor[F]): Boolean =
    F.map(fa)(identity) == fa

  def compositeLaw[F[_], A, B, C](fa: F[A], f1: A => B, f2: B => C)(implicit F: Functor[F]): Boolean =
    F.map(fa)(f2 compose f1) == F.map(F.map(fa)(f1))(f2)

  def main(args:Array[String]) = {
    val n: Option[Int] = Some(2)

    println(identityLaw(n))
    println(compositeLaw(n, (i: Int) => i * i, (i: Int) => i.toString))
  }
}

