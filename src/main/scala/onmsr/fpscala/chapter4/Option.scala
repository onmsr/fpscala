package onmsr.fpscala.chapter4

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

trait OptionImpl[+A] extends Option[A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A] with OptionImpl[A]
case object None extends Option[Nothing] with OptionImpl[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m => 
      val powXs = xs.map(x => math.pow(x-m, 2))
      mean(powXs)
    }
  }
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def Try[A](a: => A): Option[A] = try { Some(a) } catch { case e: Exception => None }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => 
      b.map(b1 => f(a1, b1))
    )
  }
  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    a.flatMap(aa => 
      b.flatMap(bb => 
        c.map(cc => f(aa, bb, cc))
      )
    )
  }
  // 内側のリフトを外に出す
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((l, r) => map2(l, r)(_ :: _))
  }
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(v => v) // よくわかんない
  }

  // リストの内部をリフト -> 内側のリフトを外に出す
  def parseInts(a: List[String]): Option[List[Int]] = {
    sequence(a.map(v => Try(v.toInt)))
  }

  // map後の値をもらうsequenceではなく、mapの前のリストと関数をもらうsequence
  // ひとつずつ適用して失敗したら、全体を失敗させる
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((l, r) => map2(f(l), r)(_ :: _))
  }
}
