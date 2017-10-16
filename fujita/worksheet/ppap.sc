
class SQL(words:List[String]) {

  private def word(name:String, prefix:String = "", suffix:String = ""):Any => SQL = v => {
    new SQL(prefix + (v match {
      case list: List[Any] => list.mkString(", ")
      case _ => v.toString
    }) + suffix :: name :: words)
  }

  def from = word("FROM")
  def where = word("WHERE")
  def equal = word("=", "'", "'")
  def select = word("SELECT")
  def in = word("IN", "(", ")")

  override def toString: String = words.reverse.mkString(" ")
}
object sql extends SQL(Nil)

sql select "*" from "users" where "id" equal 1

sql select List("id", "name", "email") from "users" where "email" equal "fujita@nulab-inc.com"

sql select List("id", "name", "email") from "users" where "id" in List(1, 2, 3, 4)


case object users
case object id

sql select "*" from users where id equal 1