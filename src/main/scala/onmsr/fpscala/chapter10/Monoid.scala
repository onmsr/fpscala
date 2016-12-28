package onmsr.fpscala.chapter10

import onmsr.fpscala.chapter3.{ Branch, Leaf, Tree }
import onmsr.fpscala.chapter7._
import onmsr.fpscala.chapter7.Par
import onmsr.fpscala.chapter8._
import onmsr.fpscala.chapter8.Prop._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stirngMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  // 演算順序を逆転したモノイドを生成する
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A =  a1 andThen a2
    def zero: A => A = v => v
  }

  // モノイドからfold計算する
  def concatenate[A](l: List[A], m: Monoid[A]): A = l.foldLeft(m.zero)(m.op)

  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B = l.foldLeft(m.zero)((a, b) => m.op(a, f(b)))

  // わからなかった
  def foldRightByFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    /**
      * foldRight(l: List[A])(f: (A, B) => B)のfのAを固定した関数のリストを作成する感じ。
      * val g: A => B => B = f.curried
      * g(g(g(g(z))))のようにすればいい
      * val l2: List[B => B] = as.map(g)
      * val m = endoMonoid[B]
      * val h: B => B = l2.foldLeft(m.zero)(m.op)
      * val result: B = h(z)
      */
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeftByFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.length match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ => {
        val (l, r) = v.splitAt(v.length/2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
    }
  }

  // わからなかった
  // バブルソート考えて、(Int, Int) => Boolean となったらいいなと考える。
  // これを、Option(Int, Int, Boolean)とする。
  // def ordered(v: IndexedSeq[Int]): Boolean = {}

  // TODO: Par.Par後でなおす
  def par[A](m: Monoid[A]): Monoid[Par.Par[A]] = new Monoid[Par.Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par.Par[A], b: Par.Par[A]) = Par.map2(a, b)(m.op)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(leftStub: String, words: Int, rightStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(x: WC, y: WC): WC = {
      (x, y) match {
        case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
        case (Stub(s1), Part(l, n, r)) => Part(s1 + l, n, r)
        case (Part(l, n, r), Stub(s2)) => Part(l, n, r + s2)
        case (Part(l1, n1, r1), Part(l2, n2, r2)) => Part(l1, n1 + (if ((r1+l2).isEmpty) 0 else 1) + n2, r2)
      }
    }
    // "" + "" => 0
    // "aaa" + "" => 1
    // "" + "bbb" => 1
    // "aaa" + "bbb" => 1
    // "aaa" + " bbb" => 2(一番端が空白の場合はない)
    // "aaa " + "bbb" => 2(一番端が空白の場合はない)
    def zero: WC = Stub("")
  }

  // わからなかった
  // この実装だと文字数数えているように見えるが、単語数えてる不思議。
  def count(s: String): Int = {
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  /**
    * 2つのモノイドを合成してタプルのモノイドにする
    */
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) = {
        val ((x1, x2), (y1, y2)) = (x, y)
        (a.op(x1, y1), b.op(x2, y2))
      }
      def zero: (A, B) = (a.zero, b.zero)
    }
  }

  /**
    * 1つのモノイドから、マップ構造の結合のモノイドに変換する
    */
  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {
      def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
        (x.keySet ++ y.keySet).map(k => {
          val (xv, yv) = (x.getOrElse(k, m.zero), y.getOrElse(k, m.zero))
          (k, m.op(xv, yv))
        }).toMap
      }
      def zero: Map[K, V] = Map[K, V]()
    }
  }

  // 結果がモノイドとなる関数のモノイドインスタンス
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      def op(x: A => B, y: A => B): A => B = a => mb.op(x(a), y(a))
      def zero: A => B = a => mb.zero
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
  }

  object Laws {
    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
      // 結合則 op(op(a, b), c) == op(a, op(b, c))
      val p1 = forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)) { p =>
        m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)
      }
      // 単位元 op(a, zero) == a
      val p2 = forAll(gen) { v =>
        m.op(v, m.zero) == v && m.op(m.zero, v) == v
      }
      p1 && p2
    }
  }
}

import scala.language.higherKinds

// TODO: '類'について考える
trait Foldable[F[_]] {
  import Monoid._
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B = foldRight(as)(m.zero)((a, b) => m.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = foldRight(as)(Nil: List[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = foldMapV(as, m)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
}

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = {
    as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m)) // わからない
    }
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f) // わからない
    }
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B = {
    as match {
      case None => m.zero
      case Some(a) => f(a)
    }
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case None => z
      case Some(a) => f(z, a)
    }
  }
}
