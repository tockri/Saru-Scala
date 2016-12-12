package fpinscala.monoids

import fpinscala.datastructures.{Branch, Leaf, Tree}

import scala.collection.mutable.ListBuffer

trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    override def zero: String = ""
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y

    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y

    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)

    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g

    val zero = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC) = (wc1, wc2) match {
      case (wc1: Stub, wc2: Stub) => wc1
      case (wc1: Stub, wc2: Part) => wc2
      case (wc1: Part, wc2: Stub) => wc1
      case (wc1: Part, wc2: Part) => Part(wc1.lStub, wc1.words + wc2.words, wc2.rStub)
    }

    val zero = Stub
  }

  def count(value: String): Int = {
    val list: ListBuffer[WC] = ListBuffer.empty[WC]

    def loop(partValue: String): Unit = {
      val middle: Int = partValue.length / 2
      val r: String = partValue.substring(0, middle)
      val l: String = partValue.substring(middle)
      if (partValue.length < 5) {
        list += Part(r, 0, l)
      } else {
        loop(r)
        loop(l)
      }
    }

    loop(value)
    val result: WC = list.result().foldLeft(wcMonoid.zero)(wcMonoid.op)
    result.asInstanceOf[Part].words
  }

  //  trait Foldable[F[_]] {
  //    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  //
  //    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  //
  //    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  //
  //    def concatenate[A](as: F[A])(m: Monoid[A]): A
  //  }
  //
  //  class FoldableList extends Foldable[List] {
  //    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  //
  //    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  //
  //    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)(mb.op(f(_),_))
  //
  //    override def concatenate[A](as: List[A])(m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  //
  //  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = foldRight(fa)(endoMonoid)((a, b) => List(a,b))

  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {

    import Monoid._

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)
  }

  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  //  object TreeFoldable extends Foldable[Tree] {
  //    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
  //      foldMap(as)(f.curried)(endoMonoid)(z)
  //
  //    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
  //      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid)(z)
  //  }

  //  object OptionFoldable extends Foldable[Option] {
  //    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
  //      as.foldRight(z)(f)
  //
  //    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
  //      as.foldLeft(z)(f)
  //
  //    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
  //      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  //  }

  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None => mb.zero
        case Some(a) => f(a)
      }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
      case None => z
      case Some(a) => f(z, a)
    }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
      case None => z
      case Some(a) => f(a, z)
    }
  }

}
