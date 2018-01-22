object Exam {
  def test(a:Option[Int]):String = {
    a match {
      case None => "None"
      case Some(n) if n < 10 => "smaller than 10"
      case _ => "greater than 10"
    }
  }

  def test2(a:Option[Int]):String = {
    a match {
      case None => "None"
      case Some(n) => if (n < 10) "smaller than 10" else "greater than 10"
      case _ => "not match"
    }
  }

  def main(args: Array[String]): Unit = {
    println(test(Some(5)))
    println(test2(Some(5)))
  }
}