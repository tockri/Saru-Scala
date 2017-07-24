package cls

/**
  *
  */
object Study {
  def main(args: Array[String]): Unit = {
    val a = new ClassA("hello!")(1)(200)
    println("ClassA created")
    a.printMe()
    val a2 = ClassA("World!")(2)(100)
    a2.printMe()

    val ap = ClassA("Foo")(1)_
    println("部分適用")

    Range(1, 10).foreach(i => {
      ap(i).printMe()
    })



  }
}
class ClassA(str:String)(num:Int)(num2:Int) {
  def printMe():Unit = {
    println(s"${str}, ${num}, ${num2}")
  }
}
object ClassA {
  def apply(str: String)(num: Int)(num2:Int): ClassA =
    new ClassA(str)(num)(num2)

}
