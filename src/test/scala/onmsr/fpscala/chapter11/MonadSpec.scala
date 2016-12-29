package onmsr.fpscala.chapter11

import org.specs2.mutable.Specification

class MonadSpec extends Specification {
  "MonadSpec" should {
    "test" in {
      true must_== false
    }
  }
  "Reader Monad" should {
    type Resource = String
    val session = "this is session"
    val stringReaderMonad = Reader.readerMonad[Resource]
    import Reader._
    def r[A](a: => A) = stringReaderMonad.unit(a)

    "simple usage" in {
      val t = stringReaderMonad.unit(1)
      t.run(session) must_== 1
    }
    "map" in {
      val t = stringReaderMonad.unit(1)
      // stringReaderMonad.map(t)(v => v+3).run(session) must_== 4
      t.map(v => v+3).run(session) must_== 4
    }
    "flatMap" in {
      val t = for {
        v1 <- r(1)
        v2 <- r(v1 + 3)
        v3 <- r(v2 * 2)
      } yield v3
      t.run(session) must_== 8
    }
  }
}
