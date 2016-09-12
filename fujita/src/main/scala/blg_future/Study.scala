package blg_future

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object FutureSample extends App {

  val s = "Hello"
  val f: Future[String] = Future {
    Thread.sleep(1000)
    println(s"[ThreadName] In Future: ${Thread.currentThread.getName}")
    s + " future!"

  }

  f.onSuccess { case s: String =>
    println(s"[ThreadName] In onSuccess: ${Thread.currentThread.getName}")
    println(s)
  }

  println(f.isCompleted) // false

  Await.ready(f, 5000 millisecond) // Hello future!

  println(s"[ThreadName] In App: ${Thread.currentThread.getName}")
  println(f.isCompleted) // true
}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.{Success, Failure, Random}

object PromiseSample extends App {
  val random = new Random()
  val promiseGetInt: Promise[Int] = Promise[Int]

  val futureGetInt: Future[Int] = promiseGetInt.success(1).future

  futureGetInt.onComplete {
    case Success(i) => println(s"Success! i: ${i}")
    case Failure(t) => println(s"Failure! t: ${t.getMessage}")
  }

  Thread.sleep(1000)
}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.Random

object CountDownLatchSample extends App {
  val indexHolder = new AtomicInteger(0)
  val random = new Random()
  val promises: Seq[Promise[Int]] = for {i <- 1 to 3} yield Promise[Int]
  val futures: Seq[Future[Int]] = for {i <- 1 to 8} yield Future[Int] {
    val waitMilliSec = random.nextInt(1000)
    Thread.sleep(waitMilliSec)
    waitMilliSec
  }
  futures.foreach { f => f.onSuccess {case waitMilliSec =>
    val index = indexHolder.getAndIncrement
    if(index < promises.length) {
      promises(index).success(waitMilliSec)
    }
  }}
  promises.foreach { p => p.future.onSuccess{ case waitMilliSec => println(waitMilliSec)}}
  Thread.sleep(5000)
}

/**
  * Created by fujita on 2016/08/31.
  */
object Study {
  def main(args:Array[String]) = {
    CountDownLatchSample.main(args)
    println("done.")
  }
}
