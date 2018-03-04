package fp.design.combinator

import fp.intro.Rng
import scala.util.{Failure, Success, Try}

sealed trait Result

sealed case class Pass() extends Result

sealed case class Fail[A](a: A) extends Result

sealed case class Exception[A](a: A, e: Throwable) extends Result

sealed case class Prp(run: (Int, Rng) => Result) {
  def map(f: Result => Result): Prp = Prp((i, rng) => f(run(i, rng)))

  def flatMap(f: Result => Prp): Prp = Prp((i, rng) => f(run(i, rng)).run(i, rng))

  def &&(that: Prp): Prp = Prp { (size: Int, rng: Rng) =>
    run(size, rng) match {
      case f@Fail(_) => f
      case e@Exception(_, _) => e
      case _ => that.run(size, rng)
    }
  }

  def ||(that: Prp): Prp = Prp { (size: Int, rng: Rng) =>
    run(size, rng) match {
      case Pass() => Pass()
      case _ => that.run(size, rng)
    }
  }

  def check(implicit size: Int = 100, rng: Rng = Rng(0)): Unit = {
    require(size > 0, "check size must be greater than zero.")
    this.run(size, rng) match {
      case Pass() => println("passed")
      case Fail(a) => println(s"case: $a failed")
      case Exception(a, e) => println(s"case: $a throws exception: $e")
    }
  }
}

object Prp {
  def forAll[A](gn: Gn[A])(f: A => Boolean): Prp = Prp((size: Int, rng: Rng) =>
    gn.many(Gn.const(size)).map(seq => seq.map(a => (a, Try(f(a))))).sample(rng).find {
      case (a, Success(false)) => true
      case (a, Failure(e)) => true
      case _ => false
    }.map {
      case (a, Success(false)) => Fail(a)
      case (a, Failure(e)) => Exception(a, e)
      case _ => Pass()
    }.getOrElse(Pass())
  )

  def throws[A, T <: Throwable](c: Class[T], gn: Gn[A])(f: A => Boolean): Prp = Prp((size: Int, rng: Rng) =>
    gn.many(Gn.const(size)).map(seq => seq.map(a => (a, Try(f(a))))).sample(rng).find {
      case (_, Success(_)) => true
      case (_, Failure(e)) if !c.isInstance(e) => true
      case _ => false
    }.map {
      case (a, Success(_)) => Fail(a)
      case (a, Failure(e)) => Exception(a, e)
      case _ => Pass()
    }.getOrElse(Pass())
  )
}
