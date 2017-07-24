package tdd

import org.scalatest.{DiagrammedAssertions, FlatSpec}

/**
  * テストクラスと1対1対応して作ってみる（とりあえず通常は1対1）
  */
class FizzBuzzSpec extends FlatSpec with DiagrammedAssertions{

  // 前準備
  val fizzbuzz = new FizzBuzz()
  /**
    * 3箇所冗長になったのでテストコードをリファクタリングしよう
    * --> new FizzBuzz()を上に出した
    */
  "convert x3 to Fizz" should "returns 'Fizz' for 3" in {
    assert("Fizz" == fizzbuzz.convert(3))
  }
  // 対称性のために増やすか？？→そうではない
  it should "returns 'Fizz' for 6" in {
    assert("Fizz" == fizzbuzz.convert(6))
  }

  "convert x5 to Buzz" should "returns 'Buzz' for 5" in {
    assert("Buzz" == fizzbuzz.convert(5))
  }

  "convert number of non x3 and x5 to string" should "returns '1' for 1" in {
    // 実行 & 検証
    assert("1" == fizzbuzz.convert(1))
    // assert("2" == fizzbuzz.convert(2))
    // さっき動いていたテストの後ろに書くとAssertion Roulette アンチパターンが発生する
    // 後片付け
  }
  /**
    * 1 assertion per test
    * この時点でちょっと冗長な気がするけどテストのリファクタリングはちょっと待つ
    * リファクタリングのときに、不安がなくなった時点でテストを意味を持つ必要最小限まで減らす
    * （このテストは消すべき）
    */
  it should "returns '2' for 2" in {
    assert("2" == fizzbuzz.convert(2))
  }
}
