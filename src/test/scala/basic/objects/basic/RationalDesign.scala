package basic.objects.basic

import java.util.Objects

import org.junit.Test

import scala.language.implicitConversions


class Rational(n: Int, d: Int) extends Ordered[Rational] {
  require(d != 0)
  private val gcd = greatestCommonDivisor(n.abs, d.abs)
  val numerator: Int = n / gcd
  val denominator: Int = d / gcd

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational = {
    new Rational(
      this.numerator * that.denominator + that.numerator * this.denominator,
      this.denominator * that.denominator
    )
  }

  def +(that: Int): Rational = new Rational(this.numerator + that * this.denominator, this.denominator)

  def -(that: Rational): Rational = {
    new Rational(
      this.numerator * that.denominator - that.numerator * this.denominator,
      this.denominator * that.denominator
    )
  }

  def -(that: Int): Rational = new Rational(this.numerator - that * this.denominator, this.denominator)

  def *(that: Rational): Rational = {
    new Rational(this.numerator * that.numerator, this.denominator * that.denominator)
  }

  def *(that: Int): Rational = new Rational(this.numerator * that, this.denominator)

  def /(that: Rational): Rational = {
    new Rational(this.numerator * that.denominator, this.denominator * that.numerator)
  }

  def /(that: Int): Rational = new Rational(this.numerator / that, this.denominator)

  def lessThan(that: Rational): Boolean = {
    this.numerator * that.denominator < that.numerator * this.denominator
  }

  def max(that: Rational): Rational = {
    if (this.lessThan(that)) that else this
  }

  override def toString: String = s"$numerator / $denominator"

  override def hashCode: Int = {
    Objects.hash(this.numerator.asInstanceOf[Integer], this.denominator.asInstanceOf[Integer])
  }

  override def equals(any: Any): Boolean = any match {
    case that: Rational => this.numerator == that.numerator && this.denominator == that.denominator
    case _ => false
  }

  private def greatestCommonDivisor(a: Int, b: Int): Int = if (b == 0) a else greatestCommonDivisor(b, a % b)

  override def compare(that: Rational): Int = (this.numerator * that.denominator) - (that.numerator * this.denominator)
}

object Rational {
  implicit def intToRational(i: Int): Rational = new Rational(i)
}

class RationalDesign {

  @Test
  def testRational(): Unit = {
    assert(new Rational(1, 6) + new Rational(1, 3) == new Rational(2, 3) - new Rational(1, 6))
    assert(new Rational(1, 6) * 2 == new Rational(1, 3))
  }


  /*Note that implicit conversions are not applicable because they are ambiguous:
   both method intToRational in package objects of type (i: Int)Rational
   and method intToRationalConflict in class RationalDesign of type (i: Int)Rational
   are possible conversion functions from Int(2) to ?{def *(x$1: ? >: Rational): ?} */
  //One-at-a-time Rule: Only one implicit is tried.
  //implicit def intToRationalConflict(i: Int): Rational = new Rational(i + 1)

  @Test
  def testImplicit(): Unit = {
    val oneThird = new Rational(1, 3)
    // use implicit conversion, must import scala.language.implicitConversions
    assert(2 * oneThird == new Rational(2, 3))
  }

  @Test
  def testOrdered(): Unit = {
    assert(new Rational(2, 5) < new Rational(3, 4))
    assert(new Rational(2, 6) == new Rational(1, 3))
  }
}
