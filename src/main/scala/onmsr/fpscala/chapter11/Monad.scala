package onmsr.fpscala.chapter11

// import onmsr.fpscala.chapter3.List
import onmsr.fpscala.chapter4._
import onmsr.fpscala.chapter5.Stream
import onmsr.fpscala.chapter7.Par._
import onmsr.fpscala.chapter8.Gen
import onmsr.fpscala.chapter9.Parsers
import scala.language.higherKinds
import scala.language.implicitConversions

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
    e match {
      case  Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](l: List[A])(f: A => B): List[B] = l.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = flatMapByJoin[A, B](ma)(f)

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](list: List[F[A]]): F[List[A]] = {
    list.foldRight(unit(List[A]()))((head, tail) => map2(head, tail)(_ :: _))
  }
  def traverse[A, B](list: List[A])(f: A => F[B]): F[List[B]] = {
    list.foldRight(unit(List[B]()))((head, tail) => map2(f(head), tail)(_ :: _))
  }
  def replicateMRecursive[A](n: Int, ma: F[A]): F[List[A]] = {
    @annotation.tailrec
    def loop(n: Int, ma: F[A])(acc: F[List[A]]): F[List[A]] = {
      if (n <= 0) acc else loop(n-1, ma)(map2(ma, acc)(_ :: _))
    }
    loop(n, ma)(unit(List[A]()))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  /** EXERCISE 11-5
   * replicateM:
   * 構造を壊さずに複製を行う
   * 
   * replicateM F=List:
   *  replicateM[A](n, ma: List[A]): List[List[A]]
   *  常にList(a1, ..., am)がn個複製される。Listの入れ子になる。要素数はn*m。
   *  
   * replicateM F=Option:
   *  replicateM[A](n, ma: Option[A]): Option[List[A]]
   *  Someのとき、Some(List(a1, ...., am))。成功しているときにaの複製を行う(n個)。
   *  Noneのとき、None。失敗しているときに失敗したまま。
   */

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // 答えと違う。間違ってるかも。
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]())) { (a, t) =>
      map2(f(a), t)((b, l) => if (b) a :: l else l)
    }
  }

  /** EXERCISE 11-6
   * TODO:
   * F = List, Option, Either, Future, Gen, Par, Parser
   */
  // 結合律
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g)) /// g(f(x))で、構造を保ちながら計算を進める感じ
  // flatMap(flatMap(x, f), g) == flatMap(x, flatMap(f, g))
  // op(op(x, y), z) == op(x, op(y, z))

  // クライスリ射
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /** EXERCISE 11-9: flatMapの観点からの結合律とcomposeの観点からの結合律の式が等価であることを証明
   *
   * compose(compose(f, g), h) == compose(f, compose(g, h))
   *
   * composeの定義、
   *     compose(f, g) == a => flatMap(f(a))(g) ・・・①
   * から、composeの結合律のcomposeをflatMapに置き換えていく
   * val f: A => F[B], val g: B => F[C], val h: C => F[D] (いらないけど考えやすいように)
   * compose(a => flatMap(f(a))(g), h) == compose(f, b => flatMap(g(b))(h))   (∵ ①)
   * a0 => flatMap({a => flatMap(f(a))(g)}(a0))(h)   ==    a => flatMap(f(a)){b => flatMap(g(b))(h)}  (∵ ①)
   * a0 => flatMap(flatMap(f(a0))(g))(h)             ==    a => flatMap(f(a)){b => flatMap(g(b))(h)}  (∵ ①)
   * 
   * fをidentityと仮定
   * a => flatMap(flatMap(a)(g))(h)             ==    a => flatMap(a){b => flatMap(g(b))(h)}
   * gをfに、hをgにおきかえて、メソッドのボディだけ比較
   * flatMap(flatMap(x)(f))(g)   ==  flatMap(x){ a => flatMap(f(a))(g) }
   * わかりやすくするため、メソッドチェーンで書き換え
   * x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
   * 解答ではこれで証明終了してるが、なんで等号なりたつかよくわからない
   * -> 勘違い。composeの結合律の証明じゃなくて、composeとflatMapの結合律が等しいことの証明だった
   */

  def flatMapByCompose[B, C](ma: F[B])(f: B => F[C]): F[C] = compose((_: Unit) => ma, f)(())

  def flatMapByCompose2[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    val f2: F[A] => F[F[B]] = fa => map(fa)(f) // mapつかっちゃだめかも
    compose(f2, identity[F[B]])(ma)
  }

  // ネストした構造をフラットにする。flatMapのflat部分。flatMap=map+flat(join)で代りにできる
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  /**
   * ↑ができるのは、以下を考えると理解しやすい。
   * def flatMap(ma: F[A])(f: A => F[B]): F[B]
   * def flatMap(ma: F[F[B]])(f: F[B] => F[B]): F[B] (∵ AをF[B]に置き換え)
   * def flatMap(ma: F[F[A]])(f: F[A] => F[A]): F[A] (∵ BをAに置き換え)
   * 本来はfでリフトする必要があるが、fでリフトしなくても構造を保てる場合には、fでリフトしないと、全体として一階層アンリフトすることに等しいみたい
   */

  def flatMapByJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def composeByJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = flatMap(ma)(f)
  }

  // def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
  //   def unit[A](a: => A) = p.succeed(a)
  //   override def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  // }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  // val listMonad = new Monad[List] {
  //   def unit[A](a: => A): List[A] = List(a)
  //   override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  // }
}

case class Id[A](value: A) {
  def unit[B](a: => B): Id[B] = Id(a)
  def map[B](f: A => B): Id[B] = flatMap(a => unit(f(value)))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

// EXERCISE 11-20
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(r => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
  }
  def ask[R]: Reader[R, R] = Reader(r => r)

  implicit def toReaderOperations[R, A](reader: Reader[R, A]): ReaderOperations[R, A] = ReaderOperations(reader)
  case class ReaderOperations[R, A](reader: Reader[R, A]) {
    def map[B](f: A => B): Reader[R, B] = readerMonad[R].map(reader)(f)
    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = readerMonad[R].flatMap(reader)(f)
  }
}
