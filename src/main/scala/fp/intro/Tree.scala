package fp.intro

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.Queue


sealed abstract class Tree[A](lc: Option[Tree[A]], v: Option[A], rc: Option[Tree[A]]) {
  override def toString: String = this match {
    case Leaf(a) => s"Leaf($a)"
    case Branch(optl, a, optr) =>
      s"Branch(${optl.map(_.toString).getOrElse("None")}, Some($a), ${optr.map(_.toString).getOrElse("None")})"
  }

  def size: Int = {
    @tailrec
    def loop(size: Int, t: Seq[Tree[A]]): Int = t match {
      case Leaf(_) :: rs => loop(size + 1, rs)
      case Branch(None, _, None) :: rs => loop(size + 1, rs)
      case Branch(Some(l), _, None) :: rs => loop(size + 1, l :: rs)
      case Branch(None, _, Some(r)) :: rs => loop(size + 1, r :: rs)
      case Branch(Some(l), _, Some(r)) :: rs => loop(size + 1, l :: r :: rs)
      case immutable.Nil => size
    }

    loop(0, Seq(this))
  }

  // should be changed to tail recursive
  def depth: Int = (lc, rc) match {
    case (None, None) => 0
    case (Some(l), None) => l.depth + 1
    case (None, Some(r)) => 1 + r.depth
    case (Some(l), Some(r)) => 1 + l.depth.max(r.depth)
  }

  def turn: Tree[A] = this match {
    case Leaf(a) => Leaf(a)
    case Branch(None, a, None) => Branch(None, a, None)
    case Branch(Some(l), a, None) => Branch(None, a, Some(l.turn))
    case Branch(None, a, Some(r)) => Branch(Some(r.turn), a, None)
    case Branch(Some(l), a, Some(r)) => Branch(Some(r.turn), a, Some(l.turn))
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(None, a, None) => Branch(None, f(a), None)
    case Branch(Some(l), a, None) => Branch(Some(l.map(f)), f(a), None)
    case Branch(None, a, Some(r)) => Branch(None, f(a), Some(r.map(f)))
    case Branch(Some(l), a, Some(r)) => Branch(Some(l.map(f)), f(a), Some(r.map(f)))
  }

  /**
    * depth first traverse
    */
  def dforeach[B](f: A => B): Unit = {
    @tailrec
    def loop(lst: Seq[Tree[A]])(f: A => B): Unit = lst match {
      case Leaf(a) :: rs => f(a); loop(rs)(f)
      case Branch(None, a, None) :: rs => f(a); loop(rs)(f)
      case Branch(Some(l), a, None) :: rs => f(a); loop(l :: rs)(f)
      case Branch(None, a, Some(r)) :: rs => f(a); loop(r :: rs)(f)
      case Branch(Some(l), a, Some(r)) :: rs => f(a); loop(l :: r :: rs)(f)
      case _ =>
    }

    loop(Seq(this))(f)
  }

  /**
    * broad first traverse
    */
  def bforeach[B](f: A => B): Unit = {
    @tailrec
    def loop(queue: Queue[Tree[A]])(f: A => B): Unit = queue.dequeueOption match {
      case Some((Leaf(a), q)) => f(a); loop(q)(f)
      case Some((Branch(None, a, None), q)) => f(a); loop(q)(f)
      case Some((Branch(Some(l), a, None), q)) => f(a); loop(q.enqueue(l))(f)
      case Some((Branch(None, a, Some(r)), q)) => f(a); loop(q.enqueue(r))(f)
      case Some((Branch(Some(l), a, Some(r)), q)) => f(a); loop(q.enqueue(l).enqueue(r))(f)
      case _ =>
    }

    loop(Queue(this))(f)
  }

}

sealed case class Leaf[A](v: A) extends Tree[A](lc = None, Some(v), rc = None)

sealed case class Branch[A](lc: Option[Tree[A]], v: A, rc: Option[Tree[A]]) extends Tree[A](lc, Some(v), rc)

object Tree {
  def max(t: Tree[Int]): Int = {
    @tailrec
    def loop(mx: Int, t: Seq[Tree[Int]]): Int = t match {
      case Leaf(v) :: rs => loop(mx.max(v), rs)
      case Branch(None, v, None) :: rs => loop(mx.max(v), rs)
      case Branch(Some(l), v, None) :: rs => loop(mx.max(v), l :: rs)
      case Branch(None, v, Some(r)) :: rs => loop(mx.max(v), r :: rs)
      case Branch(Some(l), v, Some(r)) :: rs => loop(mx.max(v), l :: r :: rs)
      case immutable.Nil => mx
    }

    loop(Int.MinValue, Seq(t))
  }
}

