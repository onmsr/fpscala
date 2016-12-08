package onmsr.fpscala.chapter2

import org.specs2.mutable.Specification
 
class MainSpec extends Specification {
  "PartialFunction" should {
    val pf = new PartialFunctionImpl() {}
    "partial1" in {
      val f = pf.partial1(3, (a: Int, b: Int) => { a + b + 1 })
      f(2) must_== 6
    }
    "curry" in {
      val f = pf.curry((a: Int, b: Int) => { a + b + 1 })
      f(3)(2) must_== 6
    }
    "uncurry" in {
      val f = pf.uncurry((a: Int) => (b: Int) => { a + b + 1 })
      f(3, 2) must_== 6
    }
    "compose" in {
      val f = (a: Int) => { a + 1 }
      val g = (a: Int) => { a * 2 }
      val h = pf.compose(f, g)
      h(3) must_== 7
    }
  }
}

