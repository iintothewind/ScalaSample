package fp.intro

case class Rng(seed: Long) {
  def nextInt: (Int, Rng) = {
    val newSeed = (Option(seed).filter(_ != 0).getOrElse(System.currentTimeMillis()) * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = Rng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

  //  def nextNonNegativeInt: (Int, Rng) = nextInt match {
  //    case (v, g) if v < 0 => (-(v + 1), g)
  //    case otherwise => otherwise
  //  }

  def nextNonNegativeInt: (Int, Rng) = Rng.nextNonNegativeInt.run(this)

  def nextInt(bound: Int): (Int, Rng) = Rng.nextInt(bound).run(this)

  def nextIntBetween(min: Int, maxExclusive: Int): (Int, Rng) = Rng.nextIntBetween(min, maxExclusive).run(this)

  //  def nextDouble: (Double, Rng) = {
  //    val (nonNegativeInt, rng) = nextNonNegativeInt
  //    (nonNegativeInt / Int.MaxValue.toDouble, rng)
  //  }

  def nextDouble: (Double, Rng) = Rng.nextDouble.run(this)

  //  def intDouble: ((Int, Double), Rng) = {
  //    val (i, rng1) = nextInt
  //    val (nonNegativeInt, rng2) = rng1.nextNonNegativeInt
  //    val (d, rng) = rng2.nextDouble
  //    ((i, d), rng)
  //  }

  def intDouble: ((Int, Double), Rng) = Rng.intDouble.run(this)

  //  def doubleInt: ((Double, Int), Rng) = {
  //    val (d, rng1) = nextDouble
  //    val (i, rng) = rng1.nextInt
  //    ((d, i), rng)
  //  }

  def doubleInt: ((Double, Int), Rng) = Rng.doubleInt.run(this)


  //  def ints(count: Int): (List[Int], Rng) = {
  //    @tailrec
  //    def loop(rnds: List[Int], rest: Int, rng: Rng): (List[Int], Rng) = rest match {
  //      case i if i <= 0 => (rnds, rng)
  //      case _ => val (i, nextRng) = rng.nextInt; loop(i :: rnds, rest - 1, nextRng)
  //    }
  //    loop(Nil, count, this)
  //  }

  def ints(count: Int): (Seq[Int], Rng) = Rng.many(Seq.fill(count)(Rng.nextInt): _*).run(this)
}

object Rng {
  def unit[A](a: A): Rand[A] = State.unit(a)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = State.flatMap(r)(f)

  //  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = rng => {
  //    val (a, nextRng) = r(rng)
  //    (f(a), nextRng)
  //  }

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = State.map(r)(f)

  //  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  //    val (a, rnga) = ra(rng)
  //    val (b, rngb) = rb(rnga)
  //    (f(a, b), rngb)
  //  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State.map2(ra, rb)(f)

  def many[A](rand: Rand[A]*): Rand[Seq[A]] = State.many(rand: _*)

  def nextInt: Rand[Int] = State(_.nextInt)

  def nextNonNegativeInt: Rand[Int] = map(nextInt) {
    case v if v < 0 => -(v + 1)
    case otherwise => otherwise
  }

  def nextInt(bound: Int): Rand[Int] = {
    require(bound > 0, "bound must be non-negative")
    flatMap(nextNonNegativeInt)(i => i % bound match {
      case mod if i + (bound - 1) - mod >= 0 => unit(mod)
      case _ => nextInt(bound)
    })
  }

  def nextIntBetween(min: Int, maxExclusive: Int): Rand[Int] = {
    require(maxExclusive - min > 0, "maxExclusive must be greater than min")
    nextInt(maxExclusive - min).map(_ + min)
  }

  def nextDouble: Rand[Double] = map(nextNonNegativeInt)(_ / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] = map2(nextNonNegativeInt, nextDouble)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(nextDouble, nextNonNegativeInt)((_, _))

}
