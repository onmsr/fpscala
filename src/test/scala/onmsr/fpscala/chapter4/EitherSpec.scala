package onmsr.fpscala.chapter4

import org.specs2.mutable.Specification
import scala.{Option => _, Either => _, _}
 
class EitherSpec extends Specification {
  "Either" should {
    "map" in {
      val a: Either[Exception, Int] = Right(1)
      a.map(v => v+1) must_== Right(2)
      val e = new Exception("test")
      val a2: Either[Exception, Int] = Left(e)
      a2.map(v => v+1) must_== Left(e)
    }

    "Try" in {
      Either.Try(100) must_== Right(100)
      val e = new Exception("test")
      Either.Try(throw e) must_== Left(e)
    }
  }
}
