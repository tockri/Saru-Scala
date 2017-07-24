package tdd

/*
1から100までの数をプリントするプログラムを書け。
ただし3の倍数のときは数の代わりに「Fizz」と、
5の倍数のときは「Buzz」とプリントし、
3と5両方の倍数の場合には「FizzBuzz」とプリントすること。
 */

/* 分割統治

- [x] 数を文字列にして返す
  - [x] 1には"1"を返す
  - [x] 2には"2"を返す

- [x] 3の倍数のときは数の代わりに「Fizz」と返す
- [x] 5の倍数のときは「Buzz」と返す
- [ ] 3と5両方の倍数の場合には「FizzBuzz」と返す

面倒なので後回し
- [ ] 1から100までの数
- [ ] プリントする
 */

/**
  *
  */
class FizzBuzz {

  def main(args: Array[String]): Unit = {

  }

  def convert(num:Int):String = {
    if (num % 3 == 0) {
      "Fizz"
    } else if (num % 5 == 0) {
      "Buzz"
    } else {
      num.toString
    }
  }

}
