package fp.pattern

import org.junit
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Prop}

/**
  * G为非空集合，如果在G上定义的二元运算 *，满足
  * *
  * （1）封闭性（Closure）：对于任意a，b∈G，有a*b∈G
  * （2）结合律（Associativity）：对于任意a，b，c∈G，有（a*b）*c=a*（b*c）
  * （3）幺元 （Identity）：存在幺元e，使得对于任意a∈G，e*a=a*e=a
  * 满足封闭性、结合律并且有幺元，则称G是一个含幺半群（Monoid）
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}


class MonoidSample {
  def stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  def intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  def intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  def booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  def booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optOrElse[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoCompose[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
  }

  def endoAndThen[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
  }

  def monoidProp[A](gen: Gen[A], m: Monoid[A]): Prop = {
    val p1 = forAll(gen)(a => a == m.op(a, m.zero) && a == m.op(m.zero, a) && m.op(a, m.zero) == m.op(m.zero, a))
    val p2 = forAll(gen, gen, gen)((x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z))
    p1 && p2
  }

  def flip[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def zero: A = m.zero

    override def op(a1: A, a2: A): A = m.op(a2, a1)
  }

  def concat[A](as: Seq[A], monoid: Monoid[A]): A = as.foldLeft(monoid.zero)(monoid.op)

  def foldMap[A, B](as: Seq[A], monoid: Monoid[B])(f: A => B): B = concat(as.map(f), monoid)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoCompose[B])(f.curried)(z)

  // use foldMap to implement foldLeft, we need flip() to change the traverse order
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, flip(endoCompose[B]))(a => b => f(b, a))(z)

  @junit.Test
  def testMonoid(): Unit = {
    monoidProp(Gen.alphaStr, stringMonoid).check
    monoidProp(Gen.listOf(Gen.choose(0, 9)), listMonoid[Int]).check
    monoidProp(Gen.choose(0, 9), intAddition).check
    monoidProp(Gen.choose(0, 9), intMultiplication).check
    monoidProp(Gen.oneOf(true, false), booleanOr).check
    monoidProp(Gen.oneOf(true, false), booleanAnd).check
    monoidProp(Gen.oneOf(None, Some(1), Some(2), Some(3)), optOrElse[Int]).check
  }

  @junit.Test
  def testEndoCompose(): Unit = {
    val gen: Gen[Int => Int] = Gen.oneOf[Int => Int]((i: Int) => i * 2, (i: Int) => i * 3, (i: Int) => i * 4, (i: Int) => i * 5)
    val p1 = forAll(gen, Gen.choose(0, 9))((a, i) => a(i) == endoCompose.op(a, endoCompose.zero)(i))
    val p2 = forAll(gen, Gen.choose(0, 9))((a, i) => endoCompose.op(a, endoCompose.zero)(i) == endoCompose.op(endoCompose.zero, a)(i))
    val p3 = forAll(gen, gen, gen, Gen.choose(0, 9))((x, y, z, i) => endoCompose.op(x, endoCompose.op(y, z))(i) == endoCompose.op(endoCompose.op(x,
      y), z)(i))
    (p1 && p2 && p3).check
  }

  @junit.Test
  def testEndoAndThen(): Unit = {
    val gen: Gen[Int => Int] = Gen.oneOf[Int => Int]((i: Int) => i * 2, (i: Int) => i * 3, (i: Int) => i * 4, (i: Int) => i * 5)
    val p1 = forAll(gen, Gen.choose(0, 9))((a, i) => a(i) == endoAndThen.op(a, endoAndThen.zero)(i))
    val p2 = forAll(gen, Gen.choose(0, 9))((a, i) => endoAndThen.op(a, endoAndThen.zero)(i) == endoAndThen.op(endoAndThen.zero, a)(i))
    val p3 = forAll(gen, gen, gen, Gen.choose(0, 9))((x, y, z, i) => endoAndThen.op(x, endoAndThen.op(y, z))(i) == endoAndThen.op(endoAndThen.op(x,
      y), z)(i))
    (p1 && p2 && p3).check
  }

  @junit.Test
  def testStringMonoid(): Unit = {
    val words: List[String] = List("Hic", "Est", "Index")
    assert(words.foldLeft(stringMonoid.zero)(stringMonoid.op) == words.foldRight(stringMonoid.zero)(stringMonoid.op))
    assert(words.foldLeft("")(_ + _) == words.foldRight("")(_ + _))
  }

  @junit.Test
  def testFold(): Unit = {
    println(foldLeft(List(1, 2, 3, 4))(List.empty[String])((s, i) => i.toString :: s))
    println(foldRight(List(1, 2, 3, 4))(List.empty[String])((i, s) => i.toString :: s))
  }

}
