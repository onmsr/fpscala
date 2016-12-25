package onmsr.fpscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, nextRng) if (n < 0) => (-n-1, nextRng)
      case (n, nextRng) => (n, nextRng)
    }
  }
  // 0.0 ~ 1.0の範囲で値を生成
  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (n, nextRng) => ((n / (Int.MaxValue.toDouble+1)), nextRng)
    }
  }

  def intDouble: Rand[(Int, Double)] = { rng =>
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // def int(rng: RNG): (Int, RNG) = rng.nextInt
  // ↑はRandを使って↓に書き換えられる。メソッド引数がなくなった場合valにもできる。
  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, nextRng) = s(rng)
      (f(a), nextRng)
    }
  }

  def doubleByMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble+1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def boolean: Rand[Boolean] = _.nextInt match { case (i, rng2) => (i % 2 == 0, rng2) }

  def intDoubleByBoth: Rand[(Int, Double)] = both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight[(List[A], RNG)]((Nil, rng)) { (rand, acc) => 
      val (tmpList, tmpRng) = acc
      val (v, nextRng) = rand(tmpRng)
      (v :: tmpList, nextRng)
    }
  }
  // この方法はわからなかった。Randを合成していくという考え方。合成は2つずつ行い、map2をつかう。
  def sequence_2[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_::_))

  def intsBySequence[A](count: Int): Rand[List[Int]] = sequence_2(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, nextRng) = f(rng)
      g(a)(nextRng)
    }
  }

  // 難しい
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a => 
      map(rb) { b => 
        f(a, b)
      }
    }
  }
}

// type State[S, +A] = S => (A, S)
// ↑を↓にするといいらしい。メソッドも追加できるようになる。
case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }
}

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  // def unit[A](a: A): State[RNG, A] = State(rng => (a, rng))
  // def unit[A](a: A): Rand[A] = State(rng => (a, rng))

  // modify, get, setちょっとむずかしい
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // copyした
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
}
