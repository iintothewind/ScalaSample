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

  def nextInt(bound: Int): (Int, Rng) = Rng.nextInt(bound)(this)

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

  def sequence[A](lst: List[Rand[A]]): Rand[List[A]] = State.sequence(lst)

  def nextInt: Rand[Int] = map(_.nextInt)(identity)

  def nextNonNegativeInt: Rand[Int] = map(_.nextInt) {
    case v if v < 0 => -(v + 1)
    case otherwise => otherwise
  }

  def nextInt(bound: Int): Rand[Int] = flatMap(nextNonNegativeInt)(i => i % bound match {
    case mod if i + (bound - 1) - mod >= 0 => unit(mod)
    case _ => nextInt(bound)
  })

  def nextDouble: Rand[Double] = map(nextNonNegativeInt)(_ / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] = map2(nextNonNegativeInt, nextDouble)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(nextDouble, nextNonNegativeInt)((_, _))

}


object State {
  def unit[S, A](a: A): State[S, A] = (a, _)

  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = state => {
    val (a, sa) = s(state)
    f(a)(sa)
  }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[S, A](lst: List[State[S, A]]): State[S, List[A]] = initState => {
    @tailrec
    def loop(as: List[A], state: S, sl: List[State[S, A]]): (List[A], S) = sl match {
      case Nil => (as, state)
      case x :: rs => val (xa, xstate) = x(state); loop(xa :: as, xstate, rs)
    }
    loop(Nil, initState, lst)
  }

  //  def get[S, A](st: State[S, A]): State[S, S] = st => (st, st)
  //
  //  def set[S, A](initState: State[S, A], s: S): State[S, Unit] = s => (Unit, initState(s)._2)
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    ???
//  }
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