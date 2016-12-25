package onmsr.fpscala.chapter9

import org.specs2.mutable.Specification

class ParsersSpec extends Specification {
  "Parsers" should {
    "test" in {

      
      true must_== false
    }
    "char" in {
      // class SimpleParser extends Parsers[String, Parser[Char]] {
      // }
      // val p:  = null
      // p.run(p.char(c))(c.toString) == Right(c)
      // p.run(p.string(s))(s) == Right(s)
      true
    }
    "listOfN" in {
      // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
      // regex("[0-9]+".r).flatMap(digit => listOfN(digit.toInt, char('a')))
      true
    }
    "map" in {
      // map(p)(a => a) == p
      true
    }
    "many" in {
      // run(map(many(char('c')))(_.length))
      true
    }
    "succeed" in {
      // run(succeed(a))(s) == Right(a)
      true
    }
    "slice" in {
      // char('a').many.slice.map(_.length) ** char('b').many.slice.map(_.length)
      true
    }
  }
}
