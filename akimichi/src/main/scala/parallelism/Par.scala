package parallelism

import java.util.concurrent._

object Study {
  def main(args:Array[String]): Unit = {
    Par.test139(1)
  }
}

object Par {
  /*
  p139
   */
  def test139(poolSize:Int): Unit = {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(poolSize)
    println(Par.equal(S)(a, fork(a)))
    println("equal computed!!")
    S.shutdown()
  }

  type Par[A] = ExecutorService => Future[A]



  private case class UnitFuture[A](get:A) extends Future[A] {
    def isDone = true
    def get(timeout:Long, units:TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning:Boolean):Boolean = false
  }
  /*
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  // 直ちにa値が得られる計算を作成
  def unit[A](a: => A):Par[A] = (es:ExecutorService) => UnitFuture(a)
  // ２つの並列計算の結果を２項関数で結合
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es:ExecutorService) => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }
  // runによる並列評価の対象としてマーク
  def fork[A](a: => Par[A]):Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  // 単に実行を遅らせるだけのfork
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // 式aをrunによる並列評価のためにラッピング
  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))
  // 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得
  def run[A](s:ExecutorService)(a:Par[A]):Future[A] = a(s)

  // map2を使った計算の例
  def sum(ints:IndexedSeq[Int]):Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse (0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }
  // 関数fを非同期で実装するためのユーティリティ
  def asyncF[A, B](f:A => B):A => Par[B] = (a:A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B):Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // 結果が得られた時にソートするように予約する
  def sortPar(parList: Par[List[Int]]):Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps:List[Par[A]]):Par[List[A]] =
    ps.foldRight(unit(List[A]()))((f:Par[A], acc) => map2(f, acc)((a, b) => (a :: b)))

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence2[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B) : Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e:ExecutorService)(p: Par[A], p2:Par[A]):Boolean = p(e).get == p2(e).get


  def choice_old[A](cond: Par[Boolean])(t:Par[A], f:Par[A]):Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)


  def choiceN_old[A](n: Par[Int])(choises: List[Par[A]]) : Par[A] =
    es => {
      val idx:Int = run(es)(n).get
      run(es)(choises(idx))
    }

  def choiceMap_old[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k:K = run(es)(key).get
      run(es)(choices(k))
    }

  def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def choice[A](cond: Par[Boolean])(t:Par[A], f:Par[A]):Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]) : Par[A] =
    chooser(n)(n => choices(n))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(k => choices(k))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get()(es)

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))
}
