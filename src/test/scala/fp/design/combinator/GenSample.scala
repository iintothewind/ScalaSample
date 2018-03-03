package fp.design.combinator

import fp.intro.{Rng, State}
import org.junit.Test


sealed case class Gn[A](state: State[Rng, A]) {
  def sample(implicit seed: Rng = Rng(0)): A = state.run(seed)._1

  def union(that: Gn[A]): Gn[A] = Gn.randBool.flatMap {
    case true => this
    case false => that
  }

  def map[B](f: A => B): Gn[B] = Gn(state.map(f))

  def flatMap[B](f: A => Gn[B]): Gn[B] = Gn(state.flatMap(f.andThen(_.state)))

  def many(size: Gn[Int]): Gn[Seq[A]] = size.flatMap(n => Gn(State.many(Seq.fill(n)(this.state): _*)))

}

object Gn {
  def const[A](a: A): Gn[A] = Gn(State.unit(a))

  def randInt: Gn[Int] = Gn(State(_.nextInt))

  def between(min: Int, maxExclusive: Int): Gn[Int] = Gn(State(_.nextIntBetween(min, maxExclusive)))

  def randDbl: Gn[Double] = Gn(State(_.nextDouble))

  def randBool: Gn[Boolean] = randInt.map(i => i % 2 match {
    case 0 => true
    case _ => false
  })

  def union[A](l: Gn[A], r: Gn[A]): Gn[A] = randBool.flatMap {
    case true => l
    case false => r
  }

  def weighted[A](l: (Gn[A], Double), r: (Gn[A], Double)): Gn[A] = randDbl.flatMap(d => if (d < (l._2 / (l._2 + r._2))) l._1 else r._1)

  def oneOf[A](source: Gn[IndexedSeq[A]]): Gn[A] = source.flatMap(xs => Gn.between(0, xs.size).map(xs(_)))

  def randSeq[A](source: Gn[IndexedSeq[A]])(implicit len: Gn[Int] = source.map(_.length)): Gn[Seq[A]] = oneOf(source).many(len)

  def randStr(source: String, size: Gn[Int])(implicit len: Gn[Int] = const(source.length)): Gn[Seq[String]] = randSeq(Gn.const(source: IndexedSeq[Char]))(len).map(xs => String.valueOf(xs.toArray)).many(size)

  def randLetters(size: Gn[Int])(len: Gn[Int]): Gn[Seq[String]] = Gn.randStr("abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", size)(len)

}

class GenSample {
  @Test
  def testUnit(): Unit = {
    Gn.const(0).sample.ensuring(_ == 0)
  }

  @Test
  def testRandInt(): Unit = {
    println(Gn.randInt.sample)
  }

  @Test
  def testBetween(): Unit = {
    Gn.between(1, 10).sample.ensuring(n => n >= 1 && n < 10)
  }

  @Test
  def testMany(): Unit = {
    Gn.between(1, 10).many(Gn.between(1, 10)).sample.ensuring(it => it.forall(n => (1 to 10).contains(n)))
  }

  @Test
  def testOneOf(): Unit = {
    "abcde".ensuring(it => it.contains(Gn.oneOf(Gn.const(it: IndexedSeq[Char])).sample))
  }

  @Test
  def testRandString(): Unit = {
    println(Gn.randLetters(Gn.between(5, 9))(Gn.between(5, 10)).sample)
  }

  @Test
  def testString(): Unit = {
    println(IndexedSeq("abc").foreach(s => println(s.getClass)))
  }

}
