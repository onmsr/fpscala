package onmsr.fpscala.chapter7


import java.util.concurrent.{ Callable, ExecutorService, Future, TimeUnit }

/**
 * [メモ]
 * 簡単な例が大事
 *
 *
 *
 */
object Par extends Parallel {
  type Par[A] = (ExecutorService) => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def isCancelled(): Boolean = true
    def isDone(): Boolean = true
    def get(timeout: Long, unit: TimeUnit): A = get
  }

  // わからなかった
  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(mayInterruptIfRunning: Boolean) = a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None => {
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val result = f(ar, br)
        cache = Some(result)
        result
      }
    }
  }

  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val (fa, fb) = (a(es), b(es))
    UnitFuture(f(fa.get, fb.get))
  }
  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val pab: Par[C => D] = map2(a, b)((a, b) => (c: C) => f(a, b, c))
    map2(pab, c)((g, c) => g(c))
  }
  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val pab: Par[C => D => E] = map2(a, b)((a, b) => (c: C) => (d: D) => f(a, b, c, d))
    val pabc: Par[D => E] = map2(pab, c)((g, c) => g(c))
    map2(pabc, d)((h, d) => h(d))
  }
  def fork[A](a: => Par[A]): Par[A] = es => {
    val task = new Callable[A] { def call = a(es).get }
    es.submit(task)
  }
  // forkの論理スレッドを実際にはフォークしない版
  def delay[A](a: => Par[A]): Par[A] = es => a(es)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map2WithTimeOut[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val (fa, fb) = (a(es), b(es))
    Map2Future(fa, fb, f) // ここでgetをしない。つまりfの適用を遅らせる。
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  def sequence[A](parList: List[Par[A]]): Par[List[A]] = {
    parList.foldRight[Par[List[A]]](unit(Nil))((l, r) => map2(l, r)(_ :: _))
  }

  // TODO: implementation
  // def sequenceBalanced[A](parList: List[Par[A]]): Par[List[A]] = { }
  // def sequence2[A](parList: List[Par[A]]): Par[List[A]] = { }

  /**
   * リストの要素を並行してフィルタリングする
   * List[A]をParにしてリストする。そのあとList[Par[List[A]]]となるので、Parを外にだしてPar[List[List[A]]]で、内側のListをflattenしてつぶす。
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // as.map(unit).foldRight[Par[List[A]]](unit(Nil))((l, r) => map2(l, r)((a, b) => if (f(a)) a :: b else b))
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => run(es)(choices(run(es)(n).get))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(condition => if (condition) 0 else 1))(List(t, f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => run(es)(choices(run(es)(key).get))

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = es => run(es)(f(run(es)(p).get))

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => run(es)(run(es)(a).get())
  }

  def flatMapByJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  def joinByFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  // def sum_10(ints: List[Int]): Par[Int] = {
  //  if (ints.size <= 1) {
  //    unit(ints.headOption.getOrElse(0))
  //  } else {
  //    val (l, r) = ints.splitAt(ints.size/2)
  //    map2(fork(sum_10(l)), fork(sum_10(r)))(_+_)
  //  }
  // }

  def sum(ints: List[Int]): Int = ints.foldLeft(0)(_+_)

  /**
   * 合計を求める(分割統治版)
   */
  def sum2(ints: List[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)  // 終了条件部分
    } else {
      val (l, r) = ints.splitAt(ints.size/2)
      sum2(l) + sum2(r) //  ←並列化するためには計算結果が必要
    }
  }
}

trait Parallel {
  type Par[A] = Par.Par[A]
  /**
   * 定数値を並列計算に昇格する
   */
  def unit[A](a: A): Par[A]
  /**
   * 2つの並列計算の結果を2項関数で結合する
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
  /**
   * 計算を並列評価の対象としてマークする。runにより強制されるまで実際には開始しない。
   */
  def fork[A](a:  => Par[A]): Par[A]
  /**
   * 未評価の計算を並列計算に昇格させて、並列評価の対象としてマークする。
   */
  def lazyUnit[A](a: => A): Par[A]
  /**
   * 並列計算を開始する
   */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A]
}

/*
 * 計算構造考える: Par
 * 条件:
 * - 並列で実行される
 * - 結果を格納できる
 * - 結果が取得できる
 *
 * メソッド:
 * - unit: 未計算のものを受取り、並列化可能な計算に変換する。一単位の並列化を実現する。
 * def unit[A](a: => A): Par[A]
 * - get: 結果を取得
 * def get[A](a: Par[A]): A
 */

/* ↑によりsum2は以下のようにかけるようになる
def sum3(ints: List[Int]): Int = {
  if (ints.size <= 1) {
    ints.headOption.getOrElse(0)
  } else {
    val (l, r) = ints.splitAt(ints.size/2)
    val sumL: Par[Int] = Par.unit(sum2(l))
    val sumR: Par[Int] = Par.unit(sum2(r))
    Par.get(sumL) + Par.get(sumR)
  }
}

 Par.get(Par.unit(sum2(l))) + Par.get(Par.unit(sum2(r)))
 ↑から、unitに副作用があることがわかる。get限定。
 getの呼び出しをさける or 最後の最後まで呼び出しを遅らせる or 非同期計算未完了で計算をすすめられるようにする。

 getの呼び出しをさける = sumの結果として、値ではなくPar[Int]を返す ということ
 ↑から↓が考えられる
  def sum4(ints: List[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0)) // ←unitになってる。unitの引数が非正格である必要はあるか?
    } else {
      val (l, r) = ints.splitAt(ints.size/2)
      // Par.unit(sum2(l)) + Par.unit(sum2(r))
      Par.map2(sum2(l), sum2(r))(_+_) // ←unitなくなってる。計算遅らせるのか?
    }
  }

 - map2: 並列計算を結合する
   1. def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
   2. def map2[A, B, C](a: => Par[A], b: Par[B])(f: (A, B) => C): Par[C]
   3. def map2[A, B, C](a: Par[A], b: => Par[B])(f: (A, B) => C): Par[C]
   4. def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]
   
   map2が正格の1の場合--------------
   sum(Seq(1, 2, 3, 4))
   ->
   map2(
     sum(Seq(1, 2)),
     sum(Seq(3, 4)),
   )
   map2(
     map2(
       sum(Seq(1)),
       sum(Seq(2)),
     )
     sum(Seq(3, 4)),
   )
   map2(
     map2(
       unit(1),
       unit(2)
     )
     sum(Seq(3, 4)),
   )
   map2(
     map2(
       unit(1),
       unit(2)
     )
     map2(
       sum(Seq(3)),
       sum(Seq(4)),
     )
   )
 ----------------------------------
 左と右を同時に計算開始したほうがよさそう。正格だと片方すべて展開するまで待つ必要がある。
 スレッド生成したくない場合もあるはず。→明示的なフォーク
 - fork: フォークする。もらった計算を遅らせる
 def fork[A](a: => Par[A]): Par[A]

 // fork版
 def sum5(ints: List[Int]): Par[Int] = {
   if (ints.size <= 1) {
     Par.unit(ints.headOption.getOrElse(0))
   } else {
     val (l, r) = ints.splitAt(ints.size/2)
     Par.map2(Par.fork(sum2(l)), Par.fork(sum2(r)))(_+_) // 明示的にフォークする。map2の引数が正格でよくなった。
   }
 }
 map2は並列計算の結合+計算遅延という役割を持っていた。
 forkにより、map2から計算を遅らせる部分を分けた。


- lazyUnit: unitとlazyUnitで選べるようにする
def unit[A](a: A): Par[A]
def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
// forkとの違いは => Aと=> Par[A]の部分。これはmap、flatMapの関係と似ている。そして、mapはflatMapで実装できるところも似てる。
   
 getをrunにして、並列計算の詳細(スレッドプールの割当方など)をまかせる
 def run[A](a: Par[A]): A
 */

/*
 * map(y)(id) == y => map(map(y)(g))(f) == map(y)(f compose g)
 * f = id, g = id
 * map(map(y)(id))(id) == map(y)(id compose id)
 * map(y)(id) == map(y)(id) (from map(y)(id) == y, id(x) == x, id(id(x)) => id(x) => x)
 * y == y
 */
