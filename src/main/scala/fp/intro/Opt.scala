package fp.intro

import scala.annotation.tailrec

sealed trait Opt[+A] {
  def isEmpty: Boolean

  def get: A

  def isDefined: Boolean = !isEmpty

  def nonEmpty: Boolean = isDefined

  def toList: List[A] = if (isEmpty) List() else get :: Nil

  def flatMap[B](f: A => Opt[B]): Opt[B] = if (isEmpty) Non else f(get)

  def getOrElse[U >: A](default: => U): U = if (isEmpty) default else get

  def orElse[B >: A](another: => Opt[B]): Opt[B] = if (isEmpty) another else this

  def filter(p: A => Boolean): Opt[A] = if (isEmpty || p(get)) this else Non

  def map[B](f: A => B): Opt[B] = this match {
    case Sme(a) => try Opt(f(a)) catch {
      case e: Exception => Non
    }
    case _ => Non
  }

  //  def map2[B, C](that: Opt[B])(f: (A, B) => C): Opt[C] = (this, that) match {
  //    case (Sme(l), Sme(r)) => try Opt(f(l, r)) catch {
  //      case e: Exception => Non
  //    }
  //    case _ => Non
  //  }

  def map2[B, C](that: Opt[B])(f: (A, B) => C): Opt[C] = for (a <- this; b <- that) yield f(a, b)
}

case object Non extends Opt[Nothing] {
  override def isEmpty: Boolean = true

  override def get: Nothing = throw new NoSuchElementException("Non.get")
}

final case class Sme[+A](x: A) extends Opt[A] {
  override def isEmpty: Boolean = false

  override def get: A = x
}

object Opt {

  import scala.language.implicitConversions

  implicit def Opt2Iterable[A](xo: Opt[A]): Iterable[A] = xo.toList

  def empty[A]: Opt[A] = Non

  def apply[A](x: A): Opt[A] = if (x == null) Non else Sme(x)

  def attempt[A](a: => A): Opt[A] = try Sme(a) catch {
    case e: Exception => Non
  }

  def lift[A, B](f: A => B): Opt[A] => Opt[B] = _.map(f)

  def sequence[A](xs: List[Opt[A]]): Opt[List[A]] = xs.foldRight(Opt(List.empty[A]))((x, result) => (x, result) match {
    case (Sme(a), Sme(lst)) => Sme(a :: lst)
    case _ => Non
  }).flatMap {
    case lst@_ :: _ => Sme(lst)
    case _ => Non
  }

  def traverse[A, B](xs: List[A])(f: A => B): Opt[List[B]] = {
    @tailrec
    def loop(xs: List[A], traversed: List[B])(f: A => B): Opt[List[B]] = (xs, traversed) match {
      case (x :: rs, lst) if Opt(x).map(f).isDefined => loop(rs, f(x) :: lst)(f)
      case (Nil, lst) if lst.nonEmpty => Sme(lst.reverse)
      case _ => Non
    }
    loop(xs, List.empty[B])(f)
  }
}

