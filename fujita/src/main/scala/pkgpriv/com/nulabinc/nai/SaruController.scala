package pkgpriv.com.nulabinc.nai

/**
  *
  */
object SaruController {
  def process():Unit = {
    val s = Saru("サル", 20)
    println(s.addAge)
  }
}
