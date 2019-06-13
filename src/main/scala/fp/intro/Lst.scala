package fp.intro

import scala.annotation.tailrec

sealed trait Lst[+A] {
  def head: A = this match {
    case Nls => throw new UnsupportedOperationException("head of empty list")
    case Cnl(a, rs) => a
  }

  def tail: Lst[A] = this match {
    case Nls => throw new UnsupportedOperationException("tail of empty list")
    case Cnl(a, rs) => rs
  }

  def headOption: Option[A] = this match {
    case Nls => None
    case Cnl(a, rs) => Some(a)
  }

  def init: Lst[A] = {
    @tailrec
    def loop(il: Lst[A], lst: Lst[A]): Lst[A] = lst match {
      case Nls => Nls
      case Cnl(a, rs) if rs == Nls => il
      case Cnl(a, rs) if rs != Nls => loop(a :: il, rs)
    }

    loop(Nls, this).reverse
  }

  def ::[U >: A](x: U): Lst[U] = Cnl(x, this)

  def :::[U >: A](nxs: Lst[U]): Lst[U] = nxs.foldRight(this.asInstanceOf[Lst[U]])(Cnl(_, _))

  def isEmpty: Boolean = this match {
    case Cnl(a, rs) => false
    case Nls => true
  }

  def nonEmpty: Boolean = !isEmpty

  def length: Int = foldLeft(0)((n, _) => n + 1)

  def drop(n: Int): Lst[A] = {
    @tailrec
    def loop(i: Int, l: Lst[A]): Lst[A] = l match {
      case Cnl(a, rs) if i > 0 => loop(i - 1, rs)
      case _ => l
    }

    loop(n, this)
  }

  def dropWhile[U >: A](p: U => Boolean): Lst[U] = {
    @tailrec
    def loop(l: Lst[U])(f: (U) => Boolean): Lst[U] = l match {
      case Cnl(a, rs) if f(a) => loop(rs)(f)
      case _ => l
    }

    loop(this)(p)
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: Lst[A], z: B)(f: (B, A) => B): B = as match {
      case Nls => z
      case Cnl(a, rs) => loop(rs, f(z, a))(f)
    }

    loop(this, z)(f)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this.reverse.foldLeft(z)((b, u) => f(u, b)) //less performance but stack safe

  def reverse: Lst[A] = foldLeft(Lst[A]())(_.::(_))

  def map[B](f: A => B): Lst[B] = this.foldRight(Lst.empty[B])((element, list) => f(element) :: list)

  def flatMap[B](f: A => Lst[B]): Lst[B] = this.foldLeft(Lst.empty[B])((list, element) => list ::: f(element))

  def filter(p: A => Boolean): Lst[A] = this.flatMap(x => if (p(x)) Lst(x) else Nls)

  def foreach[B](f: A => B): Unit = this.foldLeft(().asInstanceOf[B])((_, x) => f(x))

  def zip[B](bs: Lst[B]): Lst[(A, B)] = {
    @tailrec
    def loop(zipped: Lst[(A, B)], as: Lst[A], bs: Lst[B]): Lst[(A, B)] = (as, bs) match {
      case (Cnl(a, ars), Cnl(b, brs)) => loop((a, b) :: zipped, ars, brs)
      case _ => zipped
    }

    loop(Nls, this, bs).reverse
  }

  def zipWithIndex: Lst[(A, Int)] = {
    @tailrec
    def loop(zipped: Lst[(A, Int)], i: Int, xs: Lst[A]): Lst[(A, Int)] = xs match {
      case Nls => zipped
      case Cnl(a, rs) => loop((a, i) :: zipped, i + 1, rs)
    }

    loop(Nls, 0, this).reverse
  }

  def take(n: Int): Lst[A] = {
    @tailrec
    def loop(i: Int, taken: Lst[A], as: Lst[A]): Lst[A] = as match {
      case Cnl(a, rs) if i > 0 => loop(i - 1, a :: taken, rs)
      case _ => taken
    }

    loop(n, Nls, this).reverse
  }

  def takeWhile(p: A => Boolean): Lst[A] = {
    @tailrec
    def loop(taken: Lst[A], as: Lst[A])(p: A => Boolean): Lst[A] = as match {
      case Cnl(a, rs) if p(a) => loop(a :: taken, rs)(p)
      case _ => taken
    }

    loop(Nls, this)(p).reverse
  }

  def find(p: A => Boolean): Option[A] = {
    @tailrec
    def loop(as: Lst[A])(p: A => Boolean): Option[A] = as match {
      case Cnl(a, rs) if p(a) => Some(a)
      case Cnl(a, rs) if !p(a) => loop(rs)(p)
      case _ => None
    }

    loop(this)(p)
  }

  def exists(p: A => Boolean): Boolean = this.find(p).nonEmpty

  def forall(p: A => Boolean): Boolean = {
    @tailrec
    def loop(as: Lst[A])(p: A => Boolean): Boolean = as match {
      case Cnl(a, _) if !p(a) => false
      case Cnl(a, Nls) if p(a) => true
      case Cnl(a, rs) if p(a) => loop(rs)(p)
      case _ => false
    }

    loop(this)(p)
  }

  def contains[U >: A](sub: Lst[U]): Boolean = {
    @tailrec
    def loop(as: Lst[A], sub: Lst[U]): Boolean = as match {
      case Nls => false
      case l@Cnl(_, _) if sub.length > l.length => false
      case l@Cnl(_, _) if l.take(sub.length) == sub => true
      case _ => loop(as.drop(1), sub)
    }

    loop(this, sub)
  }
}

case object Nls extends Lst[Nothing] {
  override def toString: String = "Nil"
}

case class Cnl[+A](x: A, xs: Lst[A]) extends Lst[A] {
  override def toString: String = {
    @tailrec
    def loop(ts: String, lst: Lst[A]): String = lst match {
      case Nls => s"$ts::${Nls.toString}"
      case Cnl(a, rs) => loop(s"$ts::${a.toString}", rs)
    }

    loop("", this).drop(2)
  }
}

object Lst {
  def sum(ints: Lst[Int]): Int = ints.foldLeft(0)(_ + _)

  def product(ds: Lst[Double]): Double = ds.foldLeft(1.0D)(_ * _)

  def foldRight[A, B](xs: Lst[A], z: B)(f: (A, B) => B): B = xs match {
    case Nls => z
    case Cnl(x, rs) => f(x, foldRight(rs, z)(f)) //no tailrec, quite a failure
  }

  def empty[A]: Lst[A] = Nls

  def apply[A](as: A*): Lst[A] = if (as.isEmpty) Nls else Cnl(as.head, apply(as.tail: _*))
}

