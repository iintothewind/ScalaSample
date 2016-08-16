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

  def nextNonNegativeInt: (Int, Rng) = Rng.nextNonNegativeInt.run(this)

  def nextInt(bound: Int): (Int, Rng) = Rng.nextInt(bound).run(this)

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

  def ints(count: Int): (List[Int], Rng) = Rng.sequence(List.fill(count)(Rng.nextInt)).run(this)
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

  def nextInt: Rand[Int] = State(_.nextInt)

  def nextNonNegativeInt: Rand[Int] = map(nextInt) {
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

case class State[S, +A](run: S => (A, S)) {
  def flatMap[U >: A, B](f: U => State[S, B]): State[S, B] = State(state => {
    val (a, nst) = this.run(state)
    f(a).run(nst)
  })

  def map[B](f: A => B): State[S, B] = flatMap[A,B](a => State.unit(f(a)))

  def filter(p: A => Boolean): State[S, A] = this //to use syntactic sugar,

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] = flatMap[A,C](a => b.map(b => f(a, b)))

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))
}


object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = State(state => {
    val (a, sa) = s.run(state)
    f(a).run(sa)
  })


  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[S, A](lst: List[State[S, A]]): State[S, List[A]] = State(initState => {
    @tailrec
    def loop(as: List[A], state: S, sl: List[State[S, A]]): (List[A], S) = sl match {
      case Nil => (as, state)
      case x :: rs => val (xa, xstate) = x.run(state); loop(xa :: as, xstate, rs)
    }
    loop(Nil, initState, lst)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

sealed case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def react(input: Input): Machine = Machine.react(input).run(this)._2

  def react(inputs: List[Input]): Machine = Machine.react(inputs).run(this)._2
}

object Machine {

  def react(input: Input): State[Machine, (Int, Int)] = State[Machine, (Int, Int)](machine=> (machine, input) match {
    case (Machine(true, candies, coins), Coin) if candies > 0 => ((candies, coins + 1),Machine(locked = false, candies, coins+1))
    case (Machine(false, candies, coins), Turn) => ((candies - 1, coins),Machine(locked = true, candies - 1, coins))
    case _=>((machine.candies,machine.coins),machine)
  })

  def react(inputs: List[Input]): State[Machine, List[(Int, Int)]] = State.sequence(inputs.map(react))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State.sequence(inputs.map(react)).map(_.head)
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
  def testFlatMaps(): Unit = {
    val ns = Rng.nextInt(99).flatMap[Int,List[Int]](x =>
      Rng.nextInt(9).flatMap[Int,List[Int]](y =>
        Rng.sequence(List.fill(9)(Rng.nextInt(x))).map(xs => xs.map(_ % (y + 1)))
      )
    )
    println(ns.run(Rng(0)))
  }

  @Test
  def testForComprehensions(): Unit = {
    val ns = for {x: Int <- Rng.nextInt(99)
                  y: Int <- Rng.nextInt(9)
                  xs <- Rng.sequence(List.fill(9)(Rng.nextInt(x)))} yield xs.map(_ % (y + 1))
    println(ns.run(Rng(0)))
  }

  @Test
  def testNextIntLessThan(): Unit = {
    println(Rng.sequence(List.fill(9)(Rng.nextInt(20))).run(Rng(0)))
  }

  @Test
  def testSimulateMachine(): Unit = {
    println(Machine(locked = true, 10, 0).react(Nil))
    println(Machine(locked = true, 10, 0).react(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)))
    println(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 10, 0)))
  }


}