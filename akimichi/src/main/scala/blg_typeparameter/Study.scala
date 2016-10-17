package blg_typeparameter


/**
 * Backlog　Scala勉強会
 * 型パラメータのまとめ
 */
object Study {
  def main(args:Array[String]) = {
    // 共変の場合、スーパークラスで宣言した変数にサブクラスパラメータの値を代入できる。
    val c:Covariant[A] = Covariant(new B("共変"))
    println(c.value)
    // 不変の場合、宣言と同じでないと代入できない。
    val i:Invariant[B] = Invariant(new B("不変"))
    // val i2:Invariant[A] = Invariant(new C("不変2")) // これはエラーになる
    println(i.value)
    // 反変の場合、宣言で指定した型のスーパークラスを型パラメータに持つ値を代入できる。
    val ct:Contravariant[B] = Contravariant(new A("反変"))
    println(ct.value)
    // 上限境界の場合
    //val ub:Upper[A] = new Upper(new A("上限")) // こっちはエラーになる
    val ub2:Upper[C] = new Upper(new C("上限2"))
    // 下限境界の場合
    val lb1:Lower[A] = new Lower(new A("下限1"))
    //val lb2:Lower[C] = new Lower(new C("下限2")) // こっちがエラーになる
  }
}

// 非変の型パラメータ
case class Invariant[T](value:T)
// 共変の型パラメータ
case class Covariant[+T](value:T)
// 反変の型パラメータ
case class Contravariant[-T](value:AnyRef)
// 上限境界を持つ型パラメータ（Bのサブクラスだけを扱える）
case class Upper[T<:B](value:T)
// 下限境界を持つ型パラメータ（Bのスーパークラスだけを扱える）
case class Lower[T>:B](value:T)


// 親クラス
class A(val value:String) {
  override def toString():String = value
}
// 子クラス
class B(val bValue:String) extends A(bValue)
// 孫クラス
class C(val cValue:String) extends B(cValue)
