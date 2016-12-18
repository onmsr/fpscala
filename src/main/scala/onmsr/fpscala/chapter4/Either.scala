package onmsr.fpscala.chapter4

import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends EitherImpl[E, Nothing]
case class Right[+A](value: A) extends EitherImpl[Nothing, A]

trait EitherImpl[+E, +A] extends Either[E, A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}

trait TraversableEither {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]]
  def sequence[E, A](es: List[Either[E,A]]): Either[E,List[A]]
}

object Either extends TraversableEither {
  def Try[A](a: => A): Either[Exception, A] = {
    try { Right(a) } catch { case e: Exception => Left(e) }
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E, List[B]]](Right(Nil))((l, r) =>  f(l).map2(r)(_::_))
  }

  def sequence[E, A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(a => a)
  }
}
