package blg_implicit


/**
  * Created by fujita on 2016/08/24.
  */
object Study {
  def main(args:Array[String]) = {
    val m = PointAdditive
    val t1 = Point(1, 10)
    val t2 = Point(0, 0)
    val t3 = Point(-10, -1)
    println(m.plus(m.zero, t1) == t1)
    println(m.plus(t1, m.zero) == t1)
    println(m.plus(t1, m.plus(t2, t3)) == m.plus(m.plus(t1, t2), t3))

    val n = Rational.RationalAdditive
    println(sum(List(Rational(1, 1), Rational(2, 2))))
  }

  def sum[A](lst: List[A])(implicit m: Additive[A]) = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))

  /**
    * Pointをsumで扱うための
    */
  object PointAdditive extends Additive[Point] {
    def plus(a: Point, b: Point): Point = {
      if (eq(a, zero)) b
      else if (eq(b, zero)) a
      else Point((a.x * b.x), (a.y * b.y))
    }
    def zero: Point = Point(0, 0)
    def eq(a:Point, b:Point) = a.x == b.x && a.y == b.y
  }
}

trait Additive[A] {
  def plus(a: A, b: A): A
  def zero: A
}

case class Point(x:Int, y:Int)


case class Rational(num: Int, den: Int)

object Rational {
  implicit object RationalAdditive extends Additive[Rational] {
    def plus(a: Rational, b: Rational): Rational = {
      if (a == zero) {
        b
      } else if (b == zero) {
        a
      } else {
        Rational(a.num * b.den + b.num * a.den, a.den * b.den)
      }
    }
    def zero: Rational = Rational(0, 0)
  }
}
