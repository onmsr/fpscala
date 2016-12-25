package onmsr.fpscala.chapter9

import scala.language.higherKinds

trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String]

}
