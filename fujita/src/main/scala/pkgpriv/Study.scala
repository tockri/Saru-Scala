package pkgpriv

import pkgpriv.com.nulabinc.nai.{Saru, SaruController}

/**
  * Created by fujita on 2017/06/19.
  */
object Study {
  def main(args: Array[String]): Unit = {
    SaruController.process()
    //val s = Saru("与太郎", 18)
    //println(s.addAge)
  }
}
