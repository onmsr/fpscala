package onmsr.fpscala.chapter9

import java.util.regex._
import scala.language.higherKinds
import scala.language.implicitConversions
import onmsr.fpscala.chapter8._
import onmsr.fpscala.chapter8.Prop._
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]


  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /**
    * N回繰り返し
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def digits: Parser[String] = "[0-9]".r
  def whitespace: Parser[String] = "\\s*".r
  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)
  def double: Parser[Double] = doubleString.map(_.toDouble).label("double literal")

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = map2(slice(p), p2)((a, b) => b)
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = map2(p, slice(p2))((a, b) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] = p.map(Some(_)) or succeed(None)
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))
  def escapedQuoted: Parser[String] = token(quoted label "string literal")
  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = sep1(p,p2) or succeed(List())
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = map2(p, many(p2 *> p))(_ :: _)
  // def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] = map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) = start *> p <* stop
  def eof: Parser[String] = regex("\\z".r).label("unexpected trailing characters")
  def root[A](p: Parser[A]): Parser[A] = p <* eof

  /**
    * "aaa" | "bbb" を実現する。 asStringParser("aaa)(string)が適用されて、ParserOperators[String]となる。
    * ParserOperators[String].|(string("bbb))となる。たぶん。
    */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOperators[String] = ParserOperators(f(a))
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOperators[A] = ParserOperators[A](p)
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
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B >: A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    // def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
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
