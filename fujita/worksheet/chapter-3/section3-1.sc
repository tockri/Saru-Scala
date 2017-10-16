trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

abstract class Animal {
  val name:String
}
class Dog extends Animal {
  val name = "dog dog"
}

val la:List[Animal] = new Cons(new Dog(), Nil)
