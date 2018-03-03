package fp.design.combinator

import fp.intro.Rng
import org.junit.Test

import scala.util.{Failure, Success, Try}

sealed trait Result

sealed case class Pass() extends Result

sealed case class Fail[A](a: A) extends Result

sealed case class Exception[A](a: A, e: Throwable) extends Result

sealed case class Prp(run: (Int, Rng) => Result) {
  def map(f: Result => Result): Prp = Prp((i, rng) => f(run(i, rng)))

  def flatMap(f: Result => Prp): Prp = Prp((i, rng) => f(run(i, rng)).run(i, rng))

  def and(that: Prp): Prp = Prp { (size: Int, rng: Rng) =>
    run(size, rng) match {
      case f@Fail(_) => f
      case e@Exception(_, _) => e
      case _ => that.run(size, rng)
    }
  }

  def or(that: Prp): Prp = Prp { (size: Int, rng: Rng) =>
    run(size, rng) match {
      case Pass() => Pass()
      case _ => that.run(size, rng)
    }
  }

  def check(implicit i: Int = 100, rng: Rng = Rng(0)): Unit = {
    this.run(i, rng) match {
      case Pass() => println("passed")
      case Fail(a) => println(s"case: $a failed")
      case Exception(a, e) => println(s"case: $a throws exception: ${e.getMessage} ")
    }
  }
}

object Prp {
  def forAll[A](gen: Gn[A])(f: A => Boolean): Prp = Prp((size: Int, rng: Rng) =>
    gen.many(Gn.const(size)).map(seq => seq.map(a => (a, Try(f(a))))).sample(rng).find {
      case (a, Success(false)) => true
      case (a, Failure(e)) => true
      case _ => false
    }.map {
      case (a, Success(false)) => Fail(a)
      case (a, Failure(e)) => Exception(a, e)
      case _ => Pass()
    }.getOrElse(Pass())
  )

}

class PrpSample {
  @Test
  def testForAll(): Unit = {
    Prp.forAll(Gn.between(1, 10))(i => i >= 1 && i < 10).check
  }

  @Test
  def testAnd(): Unit = {
    val p1: Prp = Prp.forAll(Gn.between(1, 10))(i => i >= 1 && i < 10)
    val p2: Prp = Prp.forAll(Gn.oneOf(Gn.const(IndexedSeq(1, 2, 3, 4, 5))))(i => i >= 1 && i <= 5)
    (p1 and p2).check
  }

  @Test
  def testOr(): Unit = {
    val p1: Prp = Prp.forAll(Gn.between(1, 10))(i => i > 10)
    val p2: Prp = Prp.forAll(Gn.randLetters(Gn.const(5))(Gn.const(5)))(seq => seq.forall(_.length == 5))
    (p1 or p2).check
  }
}
