package fp.design.combinator

import fp.intro.{Rng, State}
import org.junit.Test

import scala.util.{Failure, Success, Try}


sealed case class Gn[A](state: State[Rng, A]) {
  def sample(seed: Int = 0): A = state.run(Rng(seed))._1

  def map[B](f: A => B): Gn[B] = Gn(state.map(f))

  //def flatMap[B](f: A => State[Rng, B]): Gn[B] = Gn(state.flatMap(f))

  def flatMap[B](f: A => Gn[B]): Gn[B] = Gn(state.flatMap(f.andThen(_.state)))
}

sealed case class Prp(succNum: Int = 0, run: Try[Boolean]) {
  def check: Either[String, Int] = run match {
    case Success(true) => Right(succNum + 1)
    case Success(false) => Left("test executed but failed")
    case Failure(t) => Left(t.getMessage)
  }

  def and(p: Prp): Prp = Prp(succNum + p.succNum + 2, run.transform(_ => p.run, _ => p.run))
}

object Gn {
  def const[A](a: A): Gn[A] = Gn(State.unit(a))

  def randInt: Gn[Int] = Gn(State(_.nextInt))

  def between(min: Int, maxExclusive: Int): Gn[Int] = Gn(State(_.nextIntBetween(min, maxExclusive)))

  def bool: Gn[Boolean] = randInt.map(i => i % 2 match {
    case 0 => true
    case _ => false
  })

  def sequence[A](n: Int)(g: Gn[A]): Gn[List[A]] = Gn(State.sequence(List.fill(n)(g.state)))
}

class GenSample {
  @Test
  def testUnit(): Unit = {
    println(Gn.const(0).sample())
  }

  @Test
  def testRandInt(): Unit = {
    println(Gn.randInt.sample())
  }

  @Test
  def testBetween(): Unit = {
    println(Gn.between(1, 10).sample())
  }

  @Test
  def testRandInts(): Unit = {
    println(Gn.sequence(9)(Gn.between(1, 10)).sample())
  }

  @Test
  def testFlatMap(): Unit = {
  }
}
