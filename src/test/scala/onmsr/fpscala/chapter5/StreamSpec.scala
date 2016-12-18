package onmsr.fpscala.chapter5

import org.specs2.mutable.Specification
 
class StreamSpec extends Specification {
  "Stream" should {
    "toList" in {
      Stream(1, 2, 3, 4, 5).toList must_== List(1, 2, 3, 4, 5)
    }
    "take" in {
      Stream(1, 2, 3, 4, 5).take(3).toList must_== List(1, 2, 3)
    }
    "takeWhile" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ < 5).toList must_== List(1, 2, 3, 4)
    }
    "drop" in {
      Stream(1, 2, 3, 4, 5).drop(3).toList must_== List(4, 5)
    }
  }
}
