package fp.intro

import org.junit.Test

import scala.annotation.tailrec
import scala.collection.immutable


sealed abstract class Tree[A](lc: Option[Tree[A]], rc: Option[Tree[A]], v: Option[A]) {
  def size: Int

  def depth: Int

  def map[B](f: A => B): Tree[B]

  def fold[B](z: B)(f: (B, A) => B): Tree[B]
}

sealed case class Leaf[A](v: A) extends Tree[A](lc = None, rc = None, Some(v)) {
  override def toString: String = s"Leaf($v)"

  override def size: Int = 1

  override def depth: Int = 0

  override def map[B](f: (A) => B): Leaf[B] = Leaf(f(v))

  override def fold[B](z: B)(f: (B, A) => B): Tree[B] = Leaf(f(z, v))
}

sealed case class Branch[A](lc: Option[Tree[A]], rc: Option[Tree[A]]) extends Tree[A](lc, rc, v = None) {
  override def toString: String = s"Branch(${if (lc.nonEmpty) lc.get.toString else "None"},${if (rc.nonEmpty) rc.get.toString else "None"})"

  override def size: Int = {
    @tailrec
    def loop(size: Int, t: Seq[Tree[A]]): Int = t match {
      case Leaf(_) :: rs => loop(size + 1, rs)
      case Branch(None, None) :: rs => loop(size + 1, rs)
      case Branch(Some(l), None) :: rs => loop(size + 1, l :: rs)
      case Branch(None, Some(r)) :: rs => loop(size + 1, r :: rs)
      case Branch(Some(l), Some(r)) :: rs => loop(size + 1, l :: r :: rs)
      case immutable.Nil => size
    }
    loop(0, Seq(this))
  }

  // should be changed to tail recursive
  override def depth: Int = (lc, rc) match {
    case (None, None) => 0
    case (Some(l), None) => l.depth + 1
    case (None, Some(r)) => 1 + r.depth
    case (Some(l), Some(r)) => 1 + l.depth.max(r.depth)
  }

  override def map[B](f: (A) => B): Branch[B] = (lc, rc) match {
    case (None, None) => Branch(None, None)
    case (Some(l), None) => Branch(Option(l.map(f)), None)
    case (None, Some(r)) => Branch(None, Option(r.map(f)))
    case (Some(l), Some(r)) => Branch(Option(l.map(f)), Option(r.map(f)))
  }

  override def fold[B](z: B)(f: (B, A) => B): Tree[B] = (lc, rc) match {
    case (None, None) => Branch(None, None)
    case (Some(l), None) => Branch(Option(l.fold(z)(f)), None)
    case (None, Some(r)) => Branch(None, Option(r.fold(z)(f)))
    case (Some(l), Some(r)) => Branch(Option(l.fold(z)(f)), Option(r.fold(z)(f)))
  }

}

object Tree {
  def max(t: Tree[Int]): Int = {
    @tailrec
    def loop(mx: Int, t: Seq[Tree[Int]]): Int = t match {
      case Leaf(v) :: rs => loop(mx.max(v), rs)
      case Branch(None, None) :: rs => loop(mx, rs)
      case Branch(Some(l), None) :: rs => loop(mx, l :: rs)
      case Branch(None, Some(r)) :: rs => loop(mx, r :: rs)
      case Branch(Some(l), Some(r)) :: rs => loop(mx, l :: r :: rs)
      case immutable.Nil => mx
    }
    loop(Int.MinValue, Seq(t))
  }
}

class TreeSample {

  @Test
  def testSize(): Unit = {
    assert(1 == Leaf(0).size)
    assert(3 == Branch(Some(Leaf(0)), Some(Leaf(1))).size)
    assert(2 == Branch(None, Some(Leaf(1))).size)
    assert(2 == Branch(Some(Leaf(1)), None).size)
    assert(4 == Branch(None, Some(Branch(Some(Leaf(1)), Some(Leaf(2))))).size)
    assert(5 == Branch(Some(Leaf(0)), Some(Branch(Some(Leaf(1)), Some(Leaf(2))))).size)
    assert(6 == Branch(Some(Branch(None, Some(Leaf(2)))), Some(Branch(Some(Leaf(3)), Some(Leaf(4))))).size)
    assert(7 == Branch(Some(Branch(Some(Leaf(1)), Some(Leaf(2)))), Some(Branch(Some(Leaf(3)), Some(Leaf(4))))).size)

  }

  @Test
  def testMaxIntTree(): Unit = {
    assert(4 == Tree.max(Branch(Some(Branch(Some(Leaf(1)), Some(Leaf(2)))), Some(Branch(Some(Leaf(3)), Some(Leaf(4)))))))
  }

  @Test
  def testDepth(): Unit = {
    println(Leaf(0).depth)
    println(Branch(Some(Leaf(0)), Some(Leaf(1))).depth)
    println(Branch(None, Some(Branch(Some(Leaf(1)), Some(Branch(Some(Leaf(0)), Some(Leaf(1))))))).depth)
    println(Branch(Some(Branch(Some(Leaf(1)), Some(Leaf(2)))), Some(Branch(Some(Leaf(3)), Some(Leaf(4))))).depth)
  }

  @Test
  def testMap(): Unit = {
    println(Leaf(0).map(v => v + 1))
    println(Branch(None, Some(Branch(Some(Leaf(1)), Some(Branch(Some(Leaf(0)), Some(Leaf(1))))))).map(v => v + 1))
  }

}