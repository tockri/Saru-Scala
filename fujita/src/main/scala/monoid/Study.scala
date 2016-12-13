package monoid

import fpinscala.datastructures.{Branch, Leaf, Tree}
import fpinscala.parallelism.Par.Par


/**
  * 10章 モノイド
  * Created by fujita on 2016/10/31.
  */
object Study {

  /**
    * 整数の加算
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  /**
    * 整数の積算
    */
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  /**
    * 論理和
    */
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  /**
    * 論理積
    */
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  /**
    * Optionの結合
    *
    * @tparam A
    * @return
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    val zero: Option[A] = None
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  def stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  /**
    * endo関数の結合
    *
    * @tparam A
    * @return
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2 // a => a1(a2(a))

    val zero: (A) => A = a => a
  }
  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }
  /**
    * リスト10-4
    */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
    * EXERSIZE 10.5
    * モノイドインスタンスを持つB型と持たないA型、A->Bの変換関数があれば
    * B型のモノイドを使ってA型の結合ができる
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  /**
    * EXERCISE 10.7
    * 平行結合
    */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }
//
//  /**
//    * EXERCISE 10.8
//    * 並列化バージョンのfoldMap
//    *
//    * @param m
//    * @tparam A
//    * @return
//    */
//  def par[A](m:Monoid[A]):Monoid[Par[A]] = new Monoid[Par[A]] {
//    override def op(a: Par[A], b: Par[A]): Par[A] = a.map2(b)(m.op)
//
//    override def zero: Par[A] = Par.unit(m.zero)
//  }
//
//  def parFoldMap[A,B](v:IndexedSeq[A], m:Monoid[B])(f: A => B):Par[B] =
//    Par.parMap(v)(f).flatMap { bs =>
//    foldMapV(bs, par(m))(b => Par.async(b))
//  }

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  /**
    * EXERCISE 10.10
    */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    /**
      * ２つのWCを結合する
      * @param a1 左側
      * @param a2 右側
      * @return
      */
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
        // 両方StubならStub
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        // 左がStubなら左側に加える
      case (Stub(c1), Part(l2, w2, r2)) => Part(c1 + l2, w2, r2)
        // 右がStubなら右側に加える
      case (Part(l1, w1, r1), Stub(c2)) => Part(l1, w1, r1 + c2)
        // 両方Partなら単語数を合計（挟まっているStubが単語になるかどうかも加味する）
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if (r1 + l2 != "") 1 else 0), r2)
    }

    /**
      * どんなWCと結合しても結果がかわらないWC=単位元
      * @return
      */
    override def zero: WC = Stub("")
  }

  def makeWCSeq(str:String):IndexedSeq[WC] = {
    str match {
      case "" => Vector(Stub(""))
      case " " => Vector(Part("", 0, ""))
      case _ if (str.length == 1) => Vector(Stub(str))
      case _ => makeWCSeq(str.substring(0, str.length / 2)) ++ makeWCSeq(str.substring(str.length / 2))
    }

  }

  def countWords(str:String):Int = {
    val wc = foldMapV(makeWCSeq(str), wcMonoid)(wc => wc)
    wc match {
      case Stub(_) => 1
      case Part(l, w, r) => (if (l.isEmpty) 0 else 1) + w + (if (r.isEmpty) 0 else 1)
    }
  }


  /**
    * EXERSIZE 10.16
    * 型Aと型Bがモノイドである場合、タプル(A, B)もモノイドである
    */
  def productMonoid[A, B](A: Monoid[A], B:Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)):(A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      def zero = (A.zero, B.zero)
    }

  /**
    * EXERCISE 10.17
    * 結果が関数となるモノイド
    */
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(a1: A => B, a2: A => B) = a => B.op(a1(a), a2(a))
      def zero = _ => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
//    as.foldLeft(Map[A, Int]())((acc:Map[A, Int], key:A) => acc.updated(key, acc.getOrElse(key, 0) + 1))

  def main(args: Array[String]): Unit = {
    println("Saru")

    println(bag(Vector("a", "rose", "is", "a", "rose")))
//    val a1: Int => Int = i => 2 * i
//    val a2: Int => Int = i => 3 * i
//    val c = endoMonoid.op(a1, a2)
//    println(c(10))

    //println(foldMapV(Vector(1, 2, 3), intAddition)(i=>i*2))

//    println(ordered(Vector(1, 2, 3, 10, 20, 23)))
//    println(ordered(Vector(3,2,1,0)))
//    println(ordered(Vector(-1, 1, 3, 20, 50)))


  }
}

/**
  * EXCERSIZE 10.12
  */
trait Foldable[F[_]] {
  import Study._
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    val m:Monoid[B => B] = endoMonoid[B]
    foldMap(as)(f.curried)(m)(z)
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  /**
    * EXCERSIZE 10.15
    * foldRightができるということはListにできるということである
    */
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())((a, l) => a::l)

}

/**
  * EXCERSIZE 10.12
  */
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

/**
  * EXCERSIZE 10.12
  */
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Study._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

/**
  * EXCERSIZE 10.12
  */
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

/**
  * EXCERSIZE 10.13
  */
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

/**
  * EXCERSIZE 10.14
  */
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

/**
  * p222. 並列解析の例
  * リスト10-5
  */
sealed trait WC

case class Stub(chars:String) extends WC

case class Part(lStub:String, words:Int, rStub:String) extends WC

/**
  * モノイド
  *
  * @tparam A
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}
