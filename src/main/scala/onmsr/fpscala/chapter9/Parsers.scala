package onmsr.fpscala.chapter9

import scala.language.higherKinds
import scala.language.implicitConversions
import onmsr.fpscala.chapter8._
import onmsr.fpscala.chapter8.Prop._
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  /**
    * N回繰り返し
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOperators[A] = ParserOperators[A](p)

  /**
    * "aaa" | "bbb" を実現する。 asStringParser("aaa)(string)が適用されて、ParserOperators[String]となる。
    * ParserOperators[String].|(string("bbb))となる。たぶん。
    */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOperators[String] = ParserOperators(f(a))
  // implicit def asParser[A, B](a: A)(implicit f: A => Parser[B]): ParserOperators[B] = ParserOperators(f(a))
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f andThen succeed)
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for { a <- p1; b <- p2 } yield f(a, b)
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // listOfNのn指定なし版(実装むずかしい)
  // manyの再帰呼出しが終わらないので、map2の第二引数を非正格に。
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def length[A](p: Parser[A]): Parser[Int] = map(many(p))(_.length)
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def slice[A](a: Parser[A]): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  // p1を実行して、p2を実行する
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = for { a <- p1; b <- p2 } yield (a, b)
  
  case class ParserOperators[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B >: A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def slice: Parser[String] = self.slice(p)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    }
    // map(p)(a => a) == p
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }
    // a * (b * c) == (a * b) * c
    def productLaw[A](a: Parser[A], b: Parser[A], c: Parser[A])(in: Gen[String]): Prop = {
      equal(a ** (b ** c), (a ** b) ** c)(in)
    }
  }
}
