def fib(n:Int): Int = {
  require(n >= 0)
  if (n <= 2) n
  else fib(n - 1) + fib(n - 2)
}

List(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5))
