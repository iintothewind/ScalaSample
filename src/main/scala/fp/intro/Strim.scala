package fp.intro

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait Strim[+A] {
  def isEmpty: Boolean = this match {
    case Cns(_, _) => false
    case _ => true
  }

  def nonEmpty: Boolean = !isEmpty

  def append[U >: A](bfe: => Strim[U]): Strim[U] = {
    @tailrec
    def loop(rest: Strim[U], appended: => Strim[U]): Strim[U] = rest match {
      case Nis => appended
      case Cns(h, t) => loop(t(), Cns(h, () => appended))
    }

    loop(bfe.reverse, this)
  }

  def head: A = this match {
    case Nis => throw new UnsupportedOperationException("head of empty Stream")
    case Cns(h, _) => h()
  }

  def tail: Strim[A] = this match {
    case Nis => throw new UnsupportedOperationException("tail of empty Stream")
    case Cns(_, t) => t()
  }

  //  def headOption: Option[A] = this match {
  //    case Cns(h, _) => Some(h())
  //    case _ => None
  //  }

  override def toString: String = {
    @tailrec
    def loop(s: String, st: Strim[A]): String = st match {
      case Nis => s"$s#::${Nis.getClass.getSimpleName}"
      case Cns(h, t) => loop(s"$s#::${h().toString}", t())
    }

    loop("", this).drop(3)
  }

  // not laziness
  def length: Int = {
    @tailrec
    def loop(n: Int, st: => Strim[A]): Int = st match {
      case Nis => n
      case Cns(_, t) => loop(n + 1, t())
    }

    loop(0, this)
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

  // not laziness
  def reverse: Strim[A] = {
    @tailrec
    def loop(rev: => Strim[A], st: => Strim[A]): Strim[A] = st match {
      case Nis => rev
      case Cns(h, t) => loop(Cns(h, () => rev), t())
    }

    loop(Nis, this)
  }

  // not laziness
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
    def loop(taken: => Strim[A], i: Int, st: => Strim[A]): Strim[A] = st match {
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

  // not laziness
  def foldLeft[B](z: => B)(f: (=> B, => A) => B): B = {
    @tailrec
    def loop(st: Strim[A], z: => B)(f: (=> B, => A) => B): B = st match {
      case Nis => z
      case Cns(h, t) => loop(t(), f(z, h()))(f)
    }

    loop(this, z)(f)
  }

  def foldRight[B](z: => B)(f: (=> A, => B) => B): B = if (isEmpty) z else f(head, tail.foldRight(z)(f))

  def exists(p: A => Boolean): Boolean = foldRight(false)((element, result) => p(element) || result)

  def forall(p: A => Boolean): Boolean = foldRight(true)((element, result) => p(element) && result)

  def takeWhile(p: A => Boolean): Strim[A] = foldRight(Strim.empty[A])((element, result) => if (p(element)) element #:: result else Nis)

  def headOption: Option[A] = foldRight(None.asInstanceOf[Option[A]])((element, result) => Some(element))

  def map[B](f: A => B): Strim[B] = foldRight(Strim.empty[B])((element, result) => f(element) #:: result)

  def flatMap[B](f: A => Strim[B]): Strim[B] = foldRight(Strim.empty[B])((element, result) => f(element) #::: result)

  def filter(p: (A) => Boolean): Strim[A] = flatMap(x => if (p(x)) Strim(x) else Nis)

  def zipWith[B, C](that: Strim[B])(f: (A, B) => C): Strim[C] = Strim.unfold((this, that)) {
    case (l #:: lst, r #:: rst) => Some(f(l, r), (lst, rst))
    case _ => None
  }

  def zip[B](that: Strim[B]) = zipWith(that)((_, _))

  def startsWith[U >: A](that: Strim[U]): Boolean = this.zip(that).length == that.length

}

case object Nis extends Strim[Nothing]

case class Cns[+A](h: () => A, t: () => Strim[A]) extends Strim[A]

class CnsWrapper[+A](st: => Strim[A]) {
  def #::[U >: A](hd: => U): Strim[U] = Strim.cons(hd, st)

  def #:::[U >: A](bfe: Strim[U]): Strim[U] = st.append(bfe)
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Strim[A] = f(z) match {
    case None => Nis
    case Some((a, s)) => a #:: unfold(s)(f)
  }

  def constant[A](a: A): Strim[A] = {
    //This is more efficient than `a#::constant(a)`
    //since it's just one object referencing itself, other than calling method recursively
    lazy val tail: Strim[A] = a #:: tail
    tail
  }

  def from(start: Int, step: Int): Strim[Int] = start #:: from(start + step, step)

  def fibonacci(n: Int): Strim[Int] = {
    @tailrec
    def loop(st: Strim[Int], i: Int, a: Int, b: Int): Strim[Int] = i match {
      case tmp if tmp < 0 => st
      case _ => loop(a #:: st, i - 1, b, a + b)
    }

    loop(Nis, n, 0, 1)
  }
}

