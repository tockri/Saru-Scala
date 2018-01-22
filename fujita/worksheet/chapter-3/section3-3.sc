import saruscala.datastructurs.{Cons, List, Nil}

// EXERCISE 3.4
def test(l:List[Int], n:Int) = {
  //println(List.mkstring(l))
  println(List.drop(l, n))
}
test(List(1, 2, 3), 1)



