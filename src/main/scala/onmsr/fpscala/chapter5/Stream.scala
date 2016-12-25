package onmsr.fpscala.chapter5

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty) // nがなくなった場合
      case Empty => Stream.empty // ストリームがなくなった場合
    }
  }
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A,  => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileByFold(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
  }

  def headOptionByFold: Option[A] = {
    foldRight[Option[A]](None)((a, _) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(Stream.cons(_, _))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val (h, t) = (head, tail)
    Cons(() => h, () => t)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }
}
