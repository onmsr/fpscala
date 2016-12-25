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
      List.foldRightByFoldLeft(List(1, 2, 3, 4), 1)(_+_) must_== 11
      List.foldRightByFoldLeft(List(1, 2, 3, 4), 1)(_*_) must_== 24
    }
    "append" in {
      List.append(List(1, 2), List()) must_== List(1, 2)
      List.append(List(1, 2), List(3, 4)) must_== List(1, 2, 3, 4)
    }
    "appendAll" in {
      List.appendAll(List(1, 2), List()) must_== List(1, 2)
      List.appendAll(List(1, 2), List(3, 4), List(5, 6)) must_== List(1, 2, 3, 4, 5, 6)
    }
    "add1" in {
      List.add1(List(1, 2)) must_== List(2, 3)
    }
    "itself" in {
      List.itself(List(1, 2, 3)) must_== List(1, 2, 3)
    }
    "doubleToString" in {
      List.doubleToString(List(1.0, 2.0, 3.0)) must_== List("1.0", "2.0", "3.0")
    }
    "map" in {
      List.map(List(1, 2, 3))((v: Int) => v+1) must_== List(2, 3, 4)
    }
    "filter" in {
      List.filter(List(1, 2, 3, 4))((v: Int) => v%2 == 0) must_== List(2, 4)
    }
    "flatMap" in {
      List.flatMap(List(1, 2, 3))(i => List(i, i)) must_== List(1, 1, 2, 2, 3, 3)
    }
    "filterByFlatMap" in {
      List.filterByFlatMap(List(1, 2, 3, 4))((v: Int) => v%2 == 0) must_== List(2, 4)
    }
    "addEachListElement" in {
      List.addEachListElement(List(1, 2, 3), List(4, 5, 6)) must_== List(5, 7, 9)
    }
    "zipWith" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => { a+b }) must_== List(5, 7, 9)
    }
    "hasSubsequence" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) must_== true
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)) must_== false
      List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)) must_== true
      List.hasSubsequence(List(1, 2, 3, 4), List(3, 5)) must_== false
    }
  }

  "Tree" should {
    "size" in {
      Tree.size(Leaf(1)) must_== 1
      Tree.size(Branch(Leaf(1), Leaf(2))) must_== 3
      Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2))) must_== 5
    }
    "maximum" in {
      Tree.maximum(Leaf(1)) must_== 1
      Tree.maximum(Branch(Leaf(1), Leaf(2))) must_== 2
      Tree.maximum(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))) must_== 8
    }
    "depth" in {
      Tree.depth(Leaf(1)) must_== 0
      Tree.depth(Branch(Leaf(1), Leaf(2))) must_== 1
      Tree.depth(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))) must_== 2
    }
    "map" in {
      Tree.map(Leaf(1))((v) => v+1) must_== Leaf(2)
      Tree.map(Branch(Leaf(1), Leaf(2)))((v) => v+1) must_== Branch(Leaf(2), Leaf(3))
      Tree.map(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2)))((v) => v+1) must_== Branch(Branch(Leaf(4), Leaf(9)), Leaf(3))
    }
    "sizeByFold" in {
      Tree.sizeByFold(Leaf(1)) must_== 1
      Tree.sizeByFold(Branch(Leaf(1), Leaf(2))) must_== 3
      Tree.sizeByFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2))) must_== 5
    }
    "maximumByFold" in {
      Tree.maximumByFold(Leaf(1)) must_== 1
      Tree.maximumByFold(Branch(Leaf(1), Leaf(2))) must_== 2
      Tree.maximumByFold(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))) must_== 8
    }
    "depthByFold" in {
      Tree.depthByFold(Leaf(1)) must_== 0
      Tree.depthByFold(Branch(Leaf(1), Leaf(2))) must_== 1
      Tree.depthByFold(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))) must_== 2
    }
    "mapByFold" in {
      Tree.mapByFold(Leaf(1))((v) => v+1) must_== Leaf(2)
      Tree.mapByFold(Branch(Leaf(1), Leaf(2)))((v) => v+1) must_== Branch(Leaf(2), Leaf(3))
      Tree.mapByFold(Branch(Branch(Leaf(3), Leaf(8)), Leaf(2)))((v) => v+1) must_== Branch(Branch(Leaf(4), Leaf(9)), Leaf(3))
    }
  }
}

