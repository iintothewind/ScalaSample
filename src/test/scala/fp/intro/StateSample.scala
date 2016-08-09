package fp.intro

import org.junit.Test

import scala.annotation.tailrec

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

  def nextNonNegativeInt: (Int, Rng) = Rng.nextNonNegativeInt(this)

  //  def nextDouble: (Double, Rng) = {
  //    val (nonNegativeInt, rng) = nextNonNegativeInt
  //    (nonNegativeInt / Int.MaxValue.toDouble, rng)
  //  }

  def nextDouble: (Double, Rng) = Rng.nextDouble(this)

  //  def intDouble: ((Int, Double), Rng) = {
  //    val (i, rng1) = nextInt
  //    val (nonNegativeInt, rng2) = rng1.nextNonNegativeInt
  //    val (d, rng) = rng2.nextDouble
  //    ((i, d), rng)
  //  }

  def intDouble: ((Int, Double), Rng) = Rng.intDouble(this)

  //  def doubleInt: ((Double, Int), Rng) = {
  //    val (d, rng1) = nextDouble
  //    val (i, rng) = rng1.nextInt
  //    ((d, i), rng)
  //  }

  def doubleInt: ((Double, Int), Rng) = Rng.doubleInt(this)


  //  def ints(count: Int): (List[Int], Rng) = {
  //    @tailrec
  //    def loop(rnds: List[Int], rest: Int, rng: Rng): (List[Int], Rng) = rest match {
  //      case i if i <= 0 => (rnds, rng)
  //      case _ => val (i, nextRng) = rng.nextInt; loop(i :: rnds, rest - 1, nextRng)
  //    }
  //    loop(Nil, count, this)
  //  }

  def ints(count: Int): (List[Int], Rng) = Rng.sequence(List.fill(count)(Rng.nextInt))(this)
}

object Rng {
  type Rand[+A] = Rng => (A, Rng)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextRng) = r(rng)
    (f(a), nextRng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)
    (f(a, b), rngb)
  }

  def sequence[A](lst: List[Rand[A]]): Rand[List[A]] = initRng => {
    @tailrec
    def loop(as: List[A], rng: Rng, rnds: List[Rand[A]]): (List[A], Rng) = rnds match {
      case Nil => (as, rng)
      case x :: rs => val (xa, xrng) = x(rng); loop(xa :: as, xrng, rs)
    }
    loop(Nil, initRng, lst)
  }

  def nextInt: Rand[Int] = map(_.nextInt)(identity)

  def nextNonNegativeInt: Rand[Int] = map(_.nextInt) {
    case v if v < 0 => -(v + 1)
    case otherwise => otherwise
  }

  def nextInt(bound: Int): Rand[Int] = map(nextNonNegativeInt)(_ % bound)

  def nextDouble: Rand[Double] = map(nextNonNegativeInt)(_ / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] = map2(nextNonNegativeInt, nextDouble)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(nextDouble, nextNonNegativeInt)((_, _))

}

class StateSample {

  @Test
  def testNextInt(): Unit = {
    assert(Rng(42L).nextInt == Rng(42L).nextInt)

  }

  @Test
  def testNonNegativeInt(): Unit = {
    assert(Rng(0).nextNonNegativeInt._1 > 0)
  }

  @Test
  def testNextDouble(): Unit = {
    Rng(0).nextDouble.ensuring(d => d._1 > 0D && d._1 < 1D)
  }

  @Test
  def testInts(): Unit = {
    println(Rng(0).ints(9)._1)
  }

  @Test
  def testNextIntLessThan(): Unit = {
    println(Rng.sequence(List.fill(9)(Rng.nextInt(20)))(Rng(0)))
  }
}
