package onmsr.fpscala.chapter3

import org.specs2.mutable.Specification
 
class MainSpec extends Specification {
  "List" should {
    "apply" in {
      List.apply(1, 2, 3) must_== Cons(1, Cons(2, Cons(3, Nil)))
    }
    "sum" in {
      List.sum(List()) must_== 0
      List.sum(List(1, 2, 3)) must_== 6
    }
    "tail" in {
      List.tail(List(1, 2, 3)) must_== List(2, 3)
      List.tail(Nil) must_== Nil
    }
    "setHead" in {
      List.setHead(5, List(1, 2, 3)) must_== List(5, 2, 3)
      List.setHead(5, Nil) must_== Nil
    }
    "drop" in {
      List.drop(List(1, 2, 3), 1) must_== List(2, 3)
      List.drop(List(1, 2, 3, 4), 2) must_== List(3, 4)
    }
    "dropWhile" in {
      List.dropWhile(List(1, 2, 3, 4))((a) => { a < 3 }) must_== List(3, 4)
      List.dropWhile(List(1, 2, 3, 4))((a) => { a < 10 }) must_== Nil
    }
    "foldRight" in {
      List.foldRight(List(1, 2, 3, 4), 1)(_+_) must_== 11
      List.foldRight(List(1, 2, 3, 4), 1)(_*_) must_== 24
    }
    "foldLeft" in {
      List.foldLeft(List(1, 2, 3, 4), 1)(_+_) must_== 11
      List.foldLeft(List(1, 2, 3, 4), 1)(_*_) must_== 24
    }
    "sumByFoldLeft" in {
      List.sumByFoldLeft(List()) must_== 0
      List.sumByFoldLeft(List(1, 2, 3)) must_== 6
    }
    "productByFoldLeft" in {
      List.productByFoldLeft(List()) must_== 1
      List.productByFoldLeft(List(1, 2, 3, 4)) must_== 24
    }
    "lengthByFoldLeft" in {
      List.lengthByFoldLeft(List()) must_== 0
      List.lengthByFoldLeft(List(1, 2, 3)) must_== 3
    }
    "reverseByFoldLeft" in {
      List.reverseByFoldLeft(List()) must_== List()
      // List.reverseByFoldLeft(List(1)) must_== List(1)
      // List.reverseByFoldLeft(List(1, 2)) must_== List(2, 1)
      // List.reverseByFoldLeft(List(1, 2, 3)) must_== List(3, 2, 1)
    }
    "foldLeftByFoldRight" in {
      List.foldLeftByFoldRight(List(1, 2, 3, 4), 1)(_+_) must_== 11
      List.foldLeftByFoldRight(List(1, 2, 3, 4), 1)(_*_) must_== 24
    }
    "append" in {
      List.append(List(1, 2), List()) must_== List(1, 2)
      List.append(List(1, 2), List(3, 4)) must_== List(1, 2, 3, 4)
    }
  }
}

