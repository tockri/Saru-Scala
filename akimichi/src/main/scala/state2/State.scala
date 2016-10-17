package state2

/**
  * 状態を持ち越すモナドの実装クラス
  */
case class State[S,+A](run:S=>(A,S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/**
  * State中で使うstaticメソッド
  */
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}



/**
  * 乱数生成メソッドを持つだけ
  */
trait RNG {
  // Stateの実装例
  type Rand[A] = State[RNG, A]

  def nextInt: (Int, RNG)

}
class Simple(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = new Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

/**
  * 実行して結果を出力するためのテスト
  */
object StateSpec {

  def main(args:Array[String]) = {

    val s = new Simple(10)

    val (i2, s2) = s.nextInt
    println(i2, s2)
    val (i3, s3) = s2.nextInt
    println(i3, s3)

  }
}