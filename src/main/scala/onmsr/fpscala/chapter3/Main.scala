package onmsr.fpscala.chapter3

//Nothing extends A
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
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], v: B)(f: (B, A) => B): B = {
    l match {
      case Cons(head, tail) => foldLeft(tail, f(v, head))(f)
      case _ => v
    }
  }
  def sumByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 0)(_+_)
  def productByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 1)(_*_)
  def lengthByFoldLeft(intList: List[Int]): Int = foldLeft(intList, 0)((a, b) => a+1)
  // わからない
  def reverseByFoldLeft[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => { Cons(h, acc) })
  // わからなかった
  def foldRightByFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  // def foldLeftByFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  //   foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  // }
  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))
  def appendAll[A](ls: List[A]*): List[A] = {
    if (ls.isEmpty) Nil else append(ls.head, appendAll(ls.tail: _*))
  }
  def appendAllByFold[A](ls: List[List[A]]): List[A] = {
    // leftとrightどっちがいいのかわからない
    foldRight(ls, List[A]())(append)
    // foldLeft(ls, List[A]())(append)
  }
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((a, b) => Cons(a+1, b))
  }
  def itself[A](l: List[A]): List[A] = {
    // Consはrightだとそのままのリスト返し、leftだとreverseする。
    foldRight(l, List[A]())(Cons(_, _))
  }
  def doubleToString[A](l: List[Double]): List[String] = {
    foldRight(l, List[String]())((h, t) => { Cons(h.toString, t) })
  }
  def map[A, B](list: List[A])(f: (A) => B): List[B] = {
    foldRight(list, List[B]())((h, t) => { Cons(f(h), t) })
  }
  // 答えから別解
  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    // bufにひとつずつfを適用したものを追加していく
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => {
        buf += f(h)
        go(t)
      }
    }
    go(l)
    List(buf.toList: _*) // こちらで作成したListに変換している
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    foldRight(list, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)
  }
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    // foldRight(list, List[B]())((h, t) => { append(f(h), t) }) appendAllByFoldのなかをつかってしまっている
    appendAllByFold(map(list)(f))
  }
  def filterByFlatMap[A](list: List[A])(f: A => Boolean): List[A] = {
    flatMap(list)(v => if (f(v)) List(v) else Nil)
  }
  def addEachListElement(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addEachListElement(t1, t2))
    }
  }
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }
  }
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
    case (_, Nil) => true
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case _ if startsWith(sup, sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
      case Nil => sub == Nil
    }
    // (sup, sub) match {
    //   case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) startsWith(t1, t2) else hasSubsequence(t1, sub)
    //   case _ => false
    // }
  }
}

/**
 * foldLeftのときは (head, tail) => {}
 * foldRightのときは (acc, head) => {}
 * と考えるとわかりやすそう
 */

/*
 * List(1, 2, 3)
 *  
 *     c
 *  1     c
 *      2   c
 *        3   nil
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


///////////////////////////////////////////////// tree
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def sizeByFold[A](t: Tree[A]): Int = fold(t)(_=>1)(1+_+_)
  def maximumByFold(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)
  def depthByFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((a, b) => 1+(a max b))
  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}
