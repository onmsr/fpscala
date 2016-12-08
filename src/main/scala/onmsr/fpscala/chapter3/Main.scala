package onmsr.fpscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(intList: List[Int]): Int = {
    intList match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }
  }
  def setHead[A](v: A, l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(v, tail)
    }
  }
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(head, tail) if (n != 0) => drop(tail, n-1)
      case _ => l
    }
  }
  def dropWhile[A](l: List[A])(f: (A) => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => l
    }
  }
  def foldRight[A, B](l: List[A], v: B)(f: (A, B) => B): B = {
    l match {
      case Cons(head, tail) => f(head, foldRight(tail, v)(f))
      case _ => v
    }
  }
  def foldLeft[A, B](l: List[A], v: B)(f: (B, A) => B): B = {
    l match {
      case Cons(head, tail) => f(foldLeft(tail, v)(f), head)
      case _ => v
    }
  }
  def sumByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 0)(_+_)
  def productByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 1)(_*_)
  def lengthByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 0)((a, b) => a+1)
  // わからない
  def reverseByFoldLeft[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => { Cons(h, acc) })
  }
  // わからなかった
  def foldLeftByFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }
}

/*
 * List(1, 2, 3)
 *  
 * - fold left
 *     fl
 *  1     f
 *      2   f
 *        3   u
 */

/*
 * List(1, 2, 3)
 *  
 * - fold left
 *     f
 *  1     f
 *      2   f
 *        3   u
 *
 * - fold right
 *           f
 *        f    1
 *     f    2
 *  u    3
 *
 *
 *
 * List(1)
 *  
 * - fold left
 *     f
 *  1     u
 *
 * - fold right
 *     f
 *  u     1
 */
