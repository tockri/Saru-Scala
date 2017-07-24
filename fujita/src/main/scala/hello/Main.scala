package hello

object HelloWorld {

  def main(args:Array[String]) {
    val year = 2017
    val month = 1
    val curDate = date(year)(month) _ // カリー化された関数の部分適用
    (1 to 31).foreach(curDate)

  }

  def date(year: Int)(month: Int)(day: Int) =
    println(s"$year/$month/$day")

  def divide(m:Int, n:Int): (Int, Int) = {
    (m / n, m % n)
  }

  def printVector(x:Int, y:Int) = println(s"x = $x, y = $y")
}

class Pair[+T1, +T2](val t1: T1, val t2: T2) {
  override def toString(): String = "(" + t1 + "," + t2 + ")"
}



class Point(val x:Int, val y:Int) {
  override def toString:String = "Point(" + 1 + "," + 2 + ")"

  override def equals(p:Any):Boolean = {
    if (p.isInstanceOf[Point]) {
      val he = p.asInstanceOf[Point]
      x == he.x && y == he.y
    } else {
      false
    }
  }


}

object Point {
  def apply(x:Int, y:Int):Point = new Point(x, y)
}

case class PathCombinator(elem:String) {
  def apply(next:String):PathCombinator = PathCombinator(elem + "/" + next)
  override def toString():String = elem
}

trait A {
  val foo: String
}

trait B extends A {
  def bar:String = foo + "World"
}

class C extends B {
  val foo = "Hello"

  def printBar(): Unit = println(bar)
}

class Cell[T](var value: T) {
  def put(newValue: T): Unit = {
    value = newValue
  }

  def get(): T = value
}
