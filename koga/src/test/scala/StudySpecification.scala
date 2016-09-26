import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import property.Study

object StringSpecification extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }
// 空文字列同士のとき通らない
//  property("concatenate") = forAll { (a: String, b: String) =>
//    (a+b).length > a.length && (a+b).length > b.length
//  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

}

object SumSpecification extends Properties("Study") {
  property("sum") = forAll { (ints:List[Int]) =>
    Study.sum(ints) == Study.sum(ints.reverse)
  }

  // 帰ってこなくなる
//  property("times") = forAll { (num:Int, cnt:Int) =>
//    val ints = List.fill(cnt)(num)
//    Study.sum(ints) == cnt * num
//  }
}