def testEither(right:Boolean):Either[Int, Int] =
  right match {
    case true => Right(1)
    case _ => Left(2)
  }


val e = testEither(false)

e match {
  case Right(i) => println(s"result is $i")
}