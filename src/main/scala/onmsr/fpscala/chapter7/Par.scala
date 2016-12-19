package onmsr.fpscala.chapter7

/**
 * [メモ]
 * 簡単な例が大事
 *
 *
 *
 */
object Par {

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

trait Par[A]

trait Parallel {
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
  def run[A](a: Par[A]): A
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





