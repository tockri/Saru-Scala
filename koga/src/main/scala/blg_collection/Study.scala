package blg_collection

/**
  * コレクションライブラリ
  */
object Study {
  def main(args: Array[String]): Unit = {
    val s = List(1, 2, 3).foldLeft(0)((x, y) => x + y)
    println(s"sum: ${s}")

    val rev = reverse(List(1, 2, 3))
    println(s"reversed: ${rev}")

    val sum2 = sum(List(1, 2, 3, 4))
    println(s"sum: ${sum2}")

    val m = mul(List(1, 2, 3, 4))
    println(s"mul: ${m}")

    val mk = mkString(List(1, 2, 3, 4, 5))(",")
    println(s"mkString: ${mk}")

    val mp = map2(List(1, 2, 3, 4, 5))(x => x.toString + "px")
    println(s"map: ${mp}")

    val comments:Option[List[Comment]] = getIssue(1).flatMap{i => getComments(i)}

    println(s"comments: ${comments}")

    val comments2:Option[List[Comment]] = for {
      i <- getIssue(1)
      cs <- getComments(i)
    } yield cs

    val notifiers:Option[List[Int]] = getIssue(1).flatMap {i:Issue =>
      getComments(i).flatMap { cs:List[Comment] =>
        getNotifiers(cs)
      }
    }

    println(s"notifiers: ${notifiers}")

    val notifiers2:Option[List[Int]] = for {
      i:Issue <- getIssue(1)
      cs:List[Comment] <- getComments(i)
      ns:List[Int] <- getNotifiers(cs)
    } yield ns

    println(s"notifiers2: ${notifiers2}")




  }
  // idに対応する課題を返す。課題が存在しなければNone
  def getIssue(id:Int):Option[Issue] = Some(Issue(id))

  // 課題のコメントを返す。コメントが追加されていなければNone
  def getComments(issue:Issue):Option[List[Comment]] = if (issue.id == 1) Some(issue.comments) else None

  // コメント一覧に登場する全ての通知先ユーザIDのリストを返す。通知先がなければNone
  def getNotifiers(comments: List[Comment]):Option[List[Int]] = Some(comments.map(c => c.notifier))



  // ひっくり返す
  def reverse[T](list: List[T]): List[T] =
    list.foldLeft(List[T]())((l: List[T], a: T) => a :: l)

  // 和
  def sum(list: List[Int]): Int = list.foldRight(0)((a, b) => a + b)

  // かける
  def mul(list: List[Int]): Int = list.foldRight(1)((a, b) => a * b)

  // mapをfoldLeftとreverseで実装する
  def map[T, U](list: List[T])(f: T => U): List[U] =
    list.foldLeft(List[U]())((l:List[U], a:T) => l ++ List(f(a)))

  def map2[T, U](list: List[T])(f: T => U): List[U] =
    list.foldRight(List[U]())((a:T, l:List[U]) => f(a) :: l)

  def filter[T](list:List[T])(f: T => Boolean):List[T] =
    list.foldLeft(List[T]())((l:List[T], a:T) => if(f(a)) a :: l else l).reverse

  def count[T](list:List[T])(f: T => Boolean):Int =
    list.foldLeft(0)((x:Int, a:T) => if(f(a)) x + 1 else x)

  // mkstring
  def mkString2[T](list: List[T])(sep: String): String =
    list.map(_.toString).reduceOption((str:String, e:String) => str + sep + e).getOrElse("")

  def mkString[T](list:List[T])(sep:String):String =
    list.foldLeft("") { (str:String, e:T) =>
      str match {
        case "" => e.toString
        case _ => str + sep + e.toString
      }
    }
}

case class Comment(notifier:Int)

case class Issue(id:Int) {
  def comments:List[Comment] = List(Comment(1), Comment(2), Comment(3))
}
