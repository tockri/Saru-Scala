// EXERSIZE2.2

def isSorted[A](as:Array[A], ordered:(A,A) => Boolean):Boolean = {
  def go(n:Int):Boolean = {
    if (n >= as.length - 1) true
    else if (ordered(as(n), as(n + 1))) false
    else go(n + 1)
  }
  go(0)
}

isSorted(Array(1, 2, 3), {(s1:Int, s2:Int) => s1 > s2})

isSorted(Array(1, 3, 2), {(s1:Int, s2:Int) => s1 > s2})

isSorted(Array(3, 2, 1), {(s1:Int, s2:Int) => s1 < s2})

// EXCERSIZE2.3
def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => {
    (b: B) => f(a, b)
  }
}
def test(a:Int, b:Double):Float = (a / b).toFloat
curry(test)(1)(2.0)


// EXCERSIZE2.4
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => {
    f(a)(b)
  }
}
def test2(a:Int)(b:Double):Float = (a / b).toFloat
uncurry(test2)(1, 2.0)

// EXCERSIZE2.5
def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

def test1(i:Int):String = i.toString

def test3(s:String):Double = s.toDouble

compose(test3, test1)(500)

