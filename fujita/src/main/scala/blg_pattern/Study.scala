package blg_pattern


/**
  * https://dwango.github.io/scala_text/case-class-and-pattern-matching.html
  * ケースクラスとパターンマッチング
  */
object Study {
  def main(args: Array[String]): Unit = {
    val A(x, y) = new A(10, 20)
    println(x)
    println(y)

    val tree: Tree = Branch(1, Branch(2, Empty, Empty), Branch(3, Empty, Empty))
    println(max2(tree))
    println(min(tree))
    println(depth(tree))
  }

  /**
    * 最大値
    * @param tree
    * @return
    */
  def max(tree: Tree): Int = tree match {
    case Branch(v, l, r) => Math.max(v, Math.max(min(l), min(r)))
    case Empty => Int.MinValue
  }

  def max2(tree:Tree):Option[Int] = tree match {
    case Branch(v, l, r) => (max2(l), max2(r)) match {
      case (Some(lv), None) => Some(Math.max(v, lv))
      case (None, Some(rv)) => Some(Math.max(v, rv))
      case (Some(lv), Some(rv)) => Some(Math.max(v, Math.max(lv, rv)))
      case (None, None) => Some(v)
    }
    case Empty => None
  }

  /**
    * 最小値
    * @param tree
    * @return
    */
  def min(tree: Tree): Int = tree match {
    case Branch(v, l, r) => Math.min(v, Math.min(min(l), min(r)))
    case Empty => Int.MaxValue
  }

  /**
    * 深さ
    * @param tree
    * @return
    */
  def depth(tree: Tree): Int = tree match {
    case Branch(v, l, r) => Math.max(depth(l), depth(r)) + 1
    case Empty => 0
  }

  def something(o:Option[Int]) = {
    val Some(x) = o
    println(x)
  }

  def matchTest(x:DayOfWeek): Int = {
    x match {
      case Sunday => 1
       case Monday => 2
       case Tuesday => 3
       case Wednesday => 4
       case Thursday => 5
       case Friday => 6
      case _ => 7
    }
  }
}

sealed abstract class Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree


/**
  * unapplyの最小構成
  * @param x
  * @param y
  */
class A(var x:Int, var y:Int) {
}
object A {
  def unapply(a:A):Option[(Int,Int)] = {
    Some(a.x, a.y)
  }
}

sealed abstract class DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek