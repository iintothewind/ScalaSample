package fp.intro

import org.junit.Test

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait Strim[+A] {
  def isEmpty: Boolean = this match {
    case Cns(_, _) => false
    case _ => true
  }

  def nonEmpty: Boolean = !isEmpty

  def head: A = this match {
    case Nis => throw new UnsupportedOperationException("head of empty Stream")
    case Cns(h, _) => h()
  }

  def tail: Strim[A] = this match {
    case Nis => throw new UnsupportedOperationException("tail of empty Stream")
    case Cns(_, t) => t()
  }

  def headOption: Option[A] = this match {
    case Cns(h, _) => Some(h())
    case _ => None
  }

  override def toString: String = {
    @tailrec
    def loop(s: String, st: Strim[A]): String = st match {
      case Nis => s"$s#::${Nis.getClass.getSimpleName}"
      case Cns(h, t) => loop(s"$s#::${h().toString}", t())
    }
    loop("", this).drop(3)
  }

  //  override def equals(that: Any): Boolean = {
  //    @tailrec
  //    def loop(left: Strim[A], right: Any): Boolean = (left, right) match {
  //      case (Nis, Nis) => true
  //      case (l: Cns[A], r: Cns[A]) if l.head == r.head => loop(l.tail, r.tail)
  //      case _ => false
  //    }
  //    loop(this, that)
  //  }

  def reverse: Strim[A] = {
    @tailrec
    def loop(rev: Strim[A], st: Strim[A]): Strim[A] = st match {
      case Nis => rev
      case Cns(h, t) => loop(Cns(h, () => rev), t())
    }
    loop(Nis, this)
  }

  def toList: List[A] = {
    @tailrec
    def loop(xs: List[A], st: Strim[A]): List[A] = st match {
      case Nis => xs
      case Cns(h, t) => loop(h() :: xs, t())
    }
    loop(Nil, this).reverse
  }

  def take(n: Int): Strim[A] = {
    @tailrec
    def loop(taken: Strim[A], i: Int, st: Strim[A]): Strim[A] = st match {
      case Cns(h, t) if i > 0 => loop(Cns(h, () => taken), i - 1, t())
      case _ => taken
    }
    loop(Nis, n, this).reverse
  }

  //  def takeWhile(p: A => Boolean): Strim[A] = {
  //    @tailrec
  //    def loop(taken: Strim[A], st: Strim[A])(p: A => Boolean): Strim[A] = st match {
  //      case Cns(h, t) if !p(h()) => loop(Cns(h, () => taken), st)(p)
  //      case _ => taken
  //    }
  //    loop(Nis, this)(p)
  //  }

  def drop(n: Int): Strim[A] = {
    @tailrec
    def loop(i: Int, st: Strim[A]): Strim[A] = st match {
      case Cns(h, t) if i > 0 => loop(i - 1, t())
      case _ => st
    }
    loop(n, this)
  }

  def dropWhile(p: A => Boolean): Strim[A] = {
    @tailrec
    def loop(st: Strim[A])(p: A => Boolean): Strim[A] = st match {
      case Cns(h, t) if !p(h()) => loop(t())(p)
      case _ => st
    }
    loop(this)(p)
  }

  def find(p: A => Boolean): Option[A] = {
    @tailrec
    def loop(st: Strim[A])(p: A => Boolean): Option[A] = st match {
      case Cns(h, t) if !p(h()) => loop(t())(p)
      case Cns(h, t) if p(h()) => Some(h())
      case _ => None
    }
    loop(this)(p)
  }

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = {
    @tailrec
    def loop(st: Strim[A], z: => B)(f: (=> B, A) => B): B = st match {
      case Nis => z
      case Cns(h, t) => loop(t(), f(z, h()))(f)
    }
    loop(this, z)(f)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = if (isEmpty) z else f(head, tail.foldRight(z)(f))

  def exists(p: A => Boolean): Boolean = foldRight(false)((element, result) => p(element) || result)

  def forall(p: A => Boolean): Boolean = foldRight(true)((element, result) => p(element) && result)

  def takeWhile(p: A => Boolean): Strim[A] = foldRight(Strim.empty[A])((element, result) => if (p(element)) element #:: result else Nis)

}

case object Nis extends Strim[Nothing]

case class Cns[+A](h: () => A, t: () => Strim[A]) extends Strim[A]

class CnsWrapper[A](strim: => Strim[A]) {
  def #::(hd: => A): Strim[A] = Strim.cons(hd, strim)
}

object #:: {
  def unapply[A](xs: Strim[A]): Option[(A, Strim[A])] = {
    if (xs.isEmpty) None else Some((xs.head, xs.tail))
  }
}

object Strim {
  implicit def cnsWrapper[A](strim: => Strim[A]): CnsWrapper[A] = new CnsWrapper[A](strim)

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
      print("a")
      "a"
    }, () => Nis)
    val h1 = x.headOption
    val h2 = x.headOption
  }

  @Test
  def testHeadOptionByLaziness(): Unit = {
    val x = {
      print("a")
      "a"
    } #:: "b" #:: Strim.empty[String]
    val h1 = x.headOption
    val h2 = x.headOption
  }

  @Test
  def testReverse(): Unit = {
    assert((2 #:: 1 #:: Strim.empty).toList == (1 #:: 2 #:: Strim.empty).reverse.toList)
  }

  @Test
  def testToList(): Unit = {
    assert(List(1, 2) == (1 #:: 2 #:: Strim.empty).toList)
  }

  @Test
  def testTake(): Unit = {
    assert(List(1, 2) == Strim(1, 2, 3).take(2).toList)
  }

  @Test
  def testFind(): Unit = {
    assert(Strim(1, 2, 3, 4, 5).find(_ == 3).nonEmpty)
  }

  @Test
  def testExists(): Unit = {
    val st = {
      print("a")
      "a"
    } #:: {
      print("b")
      "b"
    } #:: {
      print("c")
      "c"
    } #:: Strim.empty[String]
    assert(st.exists(_ == "a"))
  }

  @Test
  def testForall(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    assert(!st.forall(_ < 2))
  }

  @Test
  def testTakeWhile(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    assert(Nis == st.takeWhile(_ < -2))
  }

}
