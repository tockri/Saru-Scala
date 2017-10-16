import saruscala.datastructurs.{Cons, List, Nil}

def test(l:List[Int]) = l match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

test(List(1, 2, 3, 4, 5))

test(List(10, 20, 30))

test(Nil)

test(List(5))
