package freemonad


import scala.reflect.runtime.{universe => ru}
import ru._
import scala.language.{higherKinds, implicitConversions, reflectiveCalls}


trait Functor[F[_]] {
  def map[A, B](m: F[A])(f: A => B): F[B]
}

trait FunctorHelper {
  implicit def functorOps[F[_] : Functor, A](self: F[A]) = new {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(self)(f)
  }

  implicit val optionFunctor = new Functor[Option] {
    def map[A, B](m: Option[A])(f: A => B): Option[B] = m.map(f)
  }

  implicit def lazyMap2Functor[F[_]]: Functor[({type LM[X] = LazyMap2[F, X]})#LM] = {
    type LM[X] = LazyMap2[F, X]
    new Functor[LM] {
      def map[A, B](fa: LazyMap2[F, A])(f: A => B): LazyMap2[F, B] = fa.map(f)
    }
  }
}

abstract class HKFold[F[_]: Functor, A] extends FunctorHelper {

  def point[B](x: B): HKFold[F, B] = F0[F, B](x)
  def map[B]    (f: A => B): HKFold[F, B] = flatMap(x => point(f(x)))
  def flatMap[B](f: A => HKFold[F, B]): HKFold[F, B]
}
case class F0[F[_]: Functor, A](x: A) extends HKFold[F, A] {
  def flatMap[B](f: A => HKFold[F, B]): HKFold[F, B] = f(x)
}
case class F1[F[_]: Functor, A](x: F[A]) extends HKFold[F, A] {
  def flatMap[B](f: A => HKFold[F, B]): HKFold[F, B] = {
    F3[F, B](x.map(f))
  }
}
//case class F2[F[_]: Functor, A](x: F[F[A]]) extends HKFold[F, A]
case class F3[F[_]: Functor, A](x: F[HKFold[F, A]]) extends HKFold[F, A] {
  def flatMap[B](f: A => HKFold[F, B]): HKFold[F, B] = {
    val r = x.map{y =>
      y.flatMap(z => f(z))
    }
    F3(r)
  }
}


case class LazyMap[F[_], Start, A](x: F[Start], acc: Start => A) {
  def map[C](f: A => C): LazyMap[F, Start, C] = LazyMap(x, acc andThen f)
  def run(implicit functor: Functor[F]) = functor.map(x)(acc)
}

object LazyMap {
  def lift[F[_], A](x: F[A]) = LazyMap(x, identity[A])
}

abstract class LazyMap2[F[_], A] {lm =>
  type Start
  val start: F[Start]
  val acc: Start => A
  def map[B](f: A => B): LazyMap2[F, B] = new LazyMap2[F, B] {
    type Start = lm.Start
    val start = lm.start
    val acc = lm.acc andThen f
  }
  def run(implicit functor: Functor[F]) = functor.map(start)(acc)
}

case class MyBox1[A](x: A)

abstract class MyBox2 {
  type A
  val x: A
}

abstract class NCls {nl =>
  type Start
  val s:Start
  def getStart:Start = {
    nl.s
  }
}


/**
  * study of free monad
  * http://awekuit.hatenablog.com/entry/2014/12/04/091402
  * http://awekuit.hatenablog.com/entry/2015/01/30/172010
  */
object Study extends Helper {
  import LazyMap.lift
  type MyBox3[X] = MyBox2{ type A = X }

  def main(args:Array[String]):Unit = {
    testBox()
  }

  def myIdentity[A](x: A) : A = {
    type MyA = A
    x : MyA
  }

  def testBox(): Unit = {
    val b = new MyBox2 {
      type A = Int
      val x = 10
    }
    p(b)
    val z3: MyBox3[Int]    = new MyBox2 { type A = Int; val x = 10 }
    p(z3)

    val a = myIdentity(10)
    p(a)

    val n = new NCls {
      type Start = Int
      val s = 20
    }
    println(n.getStart)
  }

  implicit def myBoxFunctor = new Functor[MyBox1] {
    def map[A,B](fa: MyBox1[A])(f: A => B): MyBox1[B] = MyBox1(f(fa.x))
  }

  def testCompose(): Unit = {
    val f = (_: Int) * 2
    val g = (_: Int) + 5
    val h = (_: Int) * 10
    p((h compose g compose f)(3), "h(g(f(3)))")
    p((f andThen g andThen h)(3), "h(g(f(3)))")
    val l = lift(MyBox1(3)).map(f).map(g).map(h)
    p(l)
    p(l.run)
  }

  def testHKFold2(): Unit = {
    val a  = Option(Option(Option(25)))
    val b1 = a.map(x => x.map(y => y.map(z => F0[Option, Int](z):HKFold[Option, Int])))
    p(b1, "b1:")
    val b2 = b1.map(x => x.map(y => F3(y):HKFold[Option, Int]))
    p(b2, "b2:")
    val b3 = b2.map(x => F3(x): HKFold[Option, Int])
    p(b3, "b3:")
    val b4 = F3(b3): HKFold[Option, Int]
    p(b4, "b4:")
    b4.flatMap {f =>
      p(f, "f :")
      F0(f)
    }

    val x = for {
      a <- F1(Option(3))
      b <- F1(Option(5))
      c <- F1(Option(10))
      d <- F1(Option(null))
      e <- F1(Option("ohoho--"))
    } yield a * b + c + d + e
    p(x, "x :")
  }

  // 高階型の入れ子を畳んでみよう
  // Option(Option(Option(25))) のようなものと Option(25) のようなものを同じ型で扱いたい
  def testHKFold(): Unit = {
//    val f1: HKFold[Option, Int] = F1(Option(10))
//    p(f1)
//    val f2: HKFold[Option, Int] = F2(Option(Option(12)))
//    p(f2)
//    val a = Option(Option(Option(25)))
//    val b = a.map(x => F2(x):HKFold[Option, Int])
//    p(b, "b :")
//    val b2:HKFold[Option, Int] = F3[Option, Int](b)
//    p(b2, "b2:")
//    val c = Option(Option(25))
//    val d = c.map(x => F1(x):HKFold[Option, Int])
//    p(d, "d :")
  }

  /**
    * 基本中の基本
    */
  def testFooBar(): Unit = {
    println(toOption("hey, free monad"))
    println(foo(List("string foo")))
    println(foo(Option(2)))

    val a:Foo[Option, Int] = Bar[Option, Int](Option(100))
    p(a)

    val b:Foo[List, String] = Bar[List, String](List("foo", "bar", "baz"))
    p(b)

    val c:Foo[Option, String] = Bar[Option, String](Option("yeah"))
    p(c)

    val d: Foo[Option, Foo[Option, Int]] = Bar6(Some(100))
    p(d)
  }

  def toOption[A](x: A) :Option[A] = Option(x)

  def foo[F[_], A](x: F[A]): F[A] = x
}



trait Foo[F[_], A]
case class Bar[F[_], A](x: F[A]) extends Foo[F, A]
case class Bar1[F[_], A](x: F[A]) extends Foo[F, A]
case class Bar2[F[_], A](x: F[A]) extends Foo[F, F[A]]
case class Bar3[F[_], A](x: F[A]) extends Foo[F, F[F[A]]]
case class Bar4[F[_], A](x: F[A]) extends Foo[F, Int]
case class Bar5[F[_], A](x: F[A]) extends Foo[Option, Unit]
case class Bar6[F[_], A](x: F[A]) extends Foo[F, Foo[F, A]]


trait Helper extends FunctorHelper {
  def pr(a: Any, label:String = "") = {
    println(label + " " + a.getClass().toString().replaceAll("(freemonad|scala)\\.", "") + " -- " + a.toString())
  }
  def p[A:TypeTag](a: A, label:String = "") = {
    val ta = typeOf[A]
    println(label + " " + (ta.toString() + " -- " + a).replaceAll("(freemonad|scala)\\.", ""))
  }
}
