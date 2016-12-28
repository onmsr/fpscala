package onmsr.fpscala.chapter10

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {
  "MonoidSpec" should {
    "monoid test" in {
      case class Item(price: Int)
      val m = new Monoid[Item] {
        def op(x: Item, y: Item): Item = Item(x.price + y.price)
        def zero: Item = Item(0)
      }
      val i1 = Item(100)
      val i2 = Item(200)
      m.op(i1, i2) must_== Item(300)
    }

    "endoMonoid[Int]: op(f, zero) == f を満たすこと" in {
      val intEndoMonoid = Monoid.endoMonoid[Int]
      val f: Int => Int = v => v + 1
      val f2 = intEndoMonoid.op(f, intEndoMonoid.zero)
      0 to 10 map { v =>
        f(v) must_== f2(v)
      }
    }

    "mapMergeMonoid: モノイドが合成できる。マップの入れ子が簡単に計算できる。" in {
      val m: Monoid[Map[String, Map[String, Int]]] = Monoid.mapMergeMonoid(Monoid.mapMergeMonoid(Monoid.intAddition))
      val x = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
      val y = Map("o1" -> Map("i2" -> 3))
      m.op(x, y) must_== Map("o1" -> Map("i1" -> 1, "i2" -> 5))
    }

    "productMonoid: test" in {
      // 一時的なデータ構造に変換してから演算を行うイメージ
      case class Item(price: Int, price2: Int)
      object ItemAddition {
        type AccumulatorType = Tuple2[Int, Int]
        val itemAdditionMonoid: Monoid[AccumulatorType] = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
        val lift: Item => AccumulatorType = { case Item(price1, price2) => (price1, price2) }
        val unlift: AccumulatorType => Item = t => Item(price = t._1, price2 = t._2)
      }

      val l = List(Item(100, 10), Item(200, 20))
      val res = ItemAddition.unlift(Monoid.foldMap(l, ItemAddition.itemAdditionMonoid)(ItemAddition.lift))
      res must_== Item(300, 30)
    }

    "productMonoid: test2" in {
      case class Item(price: Int, price2: Int)
      val l = List(Item(100, 10), Item(200, 20))
      val m = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
      val res = Monoid.foldMap(l, m)({ case Item(price1, price2) => (price1, price2) })
      res._1 must_== 300
      res._2 must_== 30
    }

    "functionMonoid: test1" in {
      Monoid.functionMonoid[Int, Int](Monoid.intAddition).op(
        v => v,
        v => v / 100 * 5
      )(100) must_== 105
    }
    
    "bag: test" in {
      val target = Vector("a", "rose", "is", "a", "rose")
      val expected = Map("a" -> 2, "rose" -> 2, "is" -> 1)
      Monoid.bag(target) must_== expected
    }
  }
}
