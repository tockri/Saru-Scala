package pkgpriv.com.nulabinc.nai

/**
  *
  */
case class Saru(name:String, age:Int) {
  private[nai] def addAge = copy(name = name, age = age + 1)
}
