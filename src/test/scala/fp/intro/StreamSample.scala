package fp.intro

import org.junit.Test

import scala.annotation.tailrec

sealed trait Strim[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: Strim[A]

  def headOption: Option[A]

  def tailOption: Option[Strim[A]]

  def #::[B >: A](x: => B): Strim[B] = {
    lazy val head = x
    lazy val tail = this
    Cns(() => head, () => tail)
  }
}

case object Nis extends Strim[Nothing] {
  override def toString: String = this.getClass.getSimpleName

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new UnsupportedOperationException("head of empty Stream")

  override def tail: Strim[Nothing] = throw new UnsupportedOperationException("tail of empty Stream")

  override def headOption: Option[Nothing] = None

  override def tailOption: Option[Strim[Nothing]] = None
}

case class Cns[+A](h: () => A, t: () => Strim[A]) extends Strim[A] {
  override def toString: String = s"${h().toString}::${t().toString}"

  override def isEmpty: Boolean = false

  override def head: A = h()

  override def tail: Strim[A] = t()

  override def headOption: Option[A] = Some(h())

  override def tailOption: Option[Strim[A]] = Some(t())
}

object Strim {
  def cons[A](h: => A, t: => Strim[A]): Strim[A] = {
    lazy val head = h
    lazy val tail = t
    Cns(() => head, () => tail)
  }

  def empty[A]: Strim[A] = Nis

  def apply[A](xs: A*): Strim[A] = if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))
}


class StreamSample {
  @Test
  def testHeadOptionByStrictness(): Unit = {
    val x = Cns(() => {
      println("a")
      "a"
    }, () => Nis)
    val h1 = x.headOption
    val h2 = x.headOption
  }

  @Test
  def testHeadOptionByLaziness(): Unit = {
    val x = "b" #:: {
      println("a")
      "a"
    } #:: Nis
    val h1 = x.headOption
    val h2 = x.headOption
  }

}
