package fp.design.combinator

import fp.intro.{Rng, State}
import org.junit.Test

import scala.util.{Failure, Success, Try}


sealed case class Gn[A](state: State[Rng, A]) {
  def sample(implicit seed: Int = 0): A = state.run(Rng(seed))._1

  def map[B](f: A => B): Gn[B] = Gn(state.map(f))

  def flatMap[B](f: A => Gn[B]): Gn[B] = Gn(state.flatMap(f.andThen(_.state)))

  def many(size: Gn[Int]): Gn[List[A]] = size.flatMap(n => Gn(State.sequence(List.fill(n)(this.state))))

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

  def randDbl: Gn[Double] = Gn(State(_.nextDouble))

  def bool: Gn[Boolean] = randInt.map(i => i % 2 match {
    case 0 => true
    case _ => false
  })

  def union[A](l: Gn[A], r: Gn[A]): Gn[A] = bool.flatMap {
    case true => l
    case false => r
  }

  def weighted[A](l: (Gn[A], Double), r: (Gn[A], Double)): Gn[A] = randDbl.flatMap(d => if (d < (l._2 / (l._2 + r._2))) l._1 else r._1)

  def randList[A](source: Gn[Array[A]])(implicit len: Gn[Int] = source.map(_.length)): Gn[List[A]] = source.flatMap(array => Gn.between(0, array.length).map(array(_)).many(len))

  def randStr(source: String, size: Gn[Int])(implicit len: Gn[Int] = const(source.length)): Gn[List[String]] = randList(Gn.const(source.toCharArray))(len).map(lst => String.valueOf(lst.toArray)).many(size)

  def randLetters(size: Gn[Int])(len: Gn[Int]): Gn[List[String]] = Gn.randStr("abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", size)(len)
}

class GenSample {
  @Test
  def testUnit(): Unit = {
    println(Gn.const(0).sample)
  }

  @Test
  def testRandInt(): Unit = {
    println(Gn.randInt.sample)
  }

  @Test
  def testBetween(): Unit = {
    println(Gn.between(1, 10).sample)
  }

  @Test
  def testMany(): Unit = {
    println(Gn.between(1, 10).many(Gn.between(1, 10)).sample)
  }

  @Test
  def testRandString(): Unit = {
    println(Gn.randLetters(Gn.between(5, 9))(Gn.between(5, 10)).sample)
  }
}
