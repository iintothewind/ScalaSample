package fp.intro

import org.junit.Test

sealed trait Input

case object Coin extends Input

case object Turn extends Input

sealed case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def react(input: Input): Machine = Machine.react(input).run(this)._2

  def react(inputs: Input*): Machine = Machine.react(inputs: _*).run(this)._2
}

object Machine {
  def react(input: Input): State[Machine, (Int, Int)] = State[Machine, (Int, Int)](machine => (machine, input) match {
    case (Machine(true, candies, coins), Coin) if candies > 0 => ((candies, coins + 1), Machine(locked = false, candies, coins + 1))
    case (Machine(false, candies, coins), Turn) => ((candies - 1, coins), Machine(locked = true, candies - 1, coins))
    case _ => ((machine.candies, machine.coins), machine)
  })

  def react(inputs: Input*): State[Machine, Seq[(Int, Int)]] = State.many(inputs.map(react): _*)

  def simulateMachine(inputs: Input*): State[Machine, (Int, Int)] = State.many(inputs.map(react): _*).map(_.head)
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
  def testBetween(): Unit = {
    Rng(0).nextIntBetween(0, 9).ensuring(pair => pair._1 >= 0 && pair._1 < 9)
    Rng(0).nextIntBetween(0, 1).ensuring(pair => pair._1 >= 0 && pair._1 < 1)
    Rng(0).nextIntBetween(-9, 0).ensuring(pair => pair._1 >= -9 && pair._1 < 0)
    Rng(0).nextIntBetween(-9, 9).ensuring(pair => pair._1 >= -9 && pair._1 < 9)
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
    val ns: State[Rng, Seq[Int]] = Rng.nextInt(99).flatMap[Int, Seq[Int]](x =>
      Rng.nextInt(9).flatMap[Int, Seq[Int]](y =>
        Rng.many(Seq.fill(9)(Rng.nextInt(x + 1)): _*).map(xs => xs.map(_ % (y + 1)))
      )
    )
    println(ns.run(Rng(0)))
  }

  @Test
  def testForComprehensions(): Unit = {
    val ns = for {x: Int <- Rng.nextInt(99)
                  y: Int <- Rng.nextInt(9)
                  xs <- Rng.many(Seq.fill(9)(Rng.nextInt(x + 1)): _*)} yield xs.map(_ % (y + 1))
    println(ns.run(Rng(0)))
  }

  @Test
  def testNextIntLessThan(): Unit = {
    println(Rng.many(Seq.fill(9)(Rng.nextInt(20)): _*).run(Rng(0)))
  }

  @Test
  def testSimulateMachine(): Unit = {
    println(Machine(locked = true, 10, 0).react())
    println(Machine(locked = true, 10, 0).react(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    println(Machine.simulateMachine(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn).run(Machine(locked = true, 10, 0)))
  }
}