package blg_function

/**
  * Created by fujita on 2016/07/06.
  */
object Study {
  def main(args:Array[String]) = {
    println("### doubleの実行 ###")
    println(double(1, m => m * 2))
    println(double(3, m => m + 1))

    println("### swapArrayの実行 ###")
    val arr = Array(1, 2, 3, 4, 5)
    println(arr.toList)
    swapArray(arr)(1, 2)
    println(arr.toList)

    println("### joinByCommaの実行 ###")
    println(joinByComma(1, 10))

    Right(0).fold(a => {}, fb => {})
  }

  /**
    *
    * @param start
    * @param end
    * @return
    */
  def joinByComma(start:Int, end:Int):String = (start to end).mkString(",")

  /**
    * 与えられた関数を2回適用する
    * @param n
    * @param f
    * @return
    */
  def double(n: Int, f: Int => Int): Int = {
     f(f(n))
  }

  /**
    * 配列の要素を入れ替える
    * @param arr
    * @param i
    * @param j
    * @tparam T
    */
  def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
    val work = arr(i)
    arr(i) = arr(j)
    arr(j) = work
  }

  def safeStringToInt(str: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }
}

sealed trait O extends RuntimeException

object O1 extends O
object O2 extends O
