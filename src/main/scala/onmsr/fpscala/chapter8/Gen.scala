package onmsr.fpscala.chapter8

import onmsr.fpscala.chapter6.{ RNG, State }
import onmsr.fpscala.chapter5._

import Prop._
import scala.language.implicitConversions

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
case object Proved extends Result {
  def isFalsified = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }
  }
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
    }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  // むずかしい
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.state.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

/**
 * A型の値の生成方法を知っている何か
 */
case class Gen[A](sample: State[RNG, A]) {
  def state: State[RNG, A] = sample
  def map[B](f: A => B): Gen[B] = Gen(state.map(f))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(state.map2(g.state)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(state.flatMap(a => f(a).state))
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    // size.flatMap(n => flatMap(a => Gen.unit(List.fill(n)(a))))
    size.flatMap(n => Gen.listOfN(n, this))
  }
  // def listOf: SGen[List[A]] = Gen.listOf(this)
  // def listOf1: SGen[List[A]] = Gen.listOf1(this)
  def unsized: SGen[A] = SGen(_ => this)
  def **[B](g: Gen[B]): Gen[(A,B)] = this.map2(g)((_, _))
}

object Gen {
  def unit[A](a:  => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  // よくわかんない
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val thresold = g1._2.abs / (g1._2.abs + g2._2.abs)
    val s = State(RNG.double).flatMap(d => if (d < thresold) g1._1.state else g2._1.state)
    Gen(s)
  }
  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
  def map[B](f: A => B): SGen[B] = SGen(g(_).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => g(n).flatMap(m => f(m).g(n)))
  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => g(n) ** s2.g(n))
}
