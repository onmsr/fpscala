package onmsr.fpscala.chapter4

import org.specs2.mutable.Specification
import scala.{Option => _, Either => _, _}
 
class OptionSpec extends Specification {
  "Option" should {
    "sequence" in {
      Option.sequence(List()) must_== Some(List())
      Option.sequence(List(None)) must_== None
      Option.sequence(List(Some(1), Some(2), Some(3))) must_== Some(List(1, 2, 3))
      Option.sequence(List(Some(1), Some(2), None)) must_== None
    }
  }
}
