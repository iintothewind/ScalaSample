package fp.pattern

import org.junit
import org.junit.Ignore
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

  def endoAndThen[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
  }

  def monoidProp[A](gen: Gen[A], monoid: Monoid[A]): Prop = {
    val p1 = forAll(gen)(a => a == monoid.op(a, monoid.zero) && monoid.op(a, monoid.zero) == monoid.op(monoid.zero, a))
    val p2 = forAll(gen, gen, gen)((x, y, z) => monoid.op(x, monoid.op(y, z)) == monoid.op(monoid.op(x, y), z))
    p1 && p2
  }

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

  @Ignore
  @junit.Test
  def testEndoAndThen(): Unit = {
    monoidProp(Gen.oneOf[Int => Int]((i: Int) => i * 2, (i: Int) => i * 3, (i: Int) => i * 4, (i: Int) => i * 5), endoAndThen[Int]).check
  }
}
