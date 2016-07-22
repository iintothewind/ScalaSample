package fp.intro

import org.junit.Test

import scala.annotation.tailrec

sealed trait List[+A] {
  def head(): A

  def tail(): List[A]

  def init(): List[A]

  def ::[B >: A](x: B): List[B]

  def :::[B >: A](nxs: List[B]): List[B]

  def isEmpty: Boolean

  def length: Int

  def setHead[B >: A](newHead: B): List[B]

  def drop(n: Int): List[A]

  def dropWhile[B >: A](f: B => Boolean): List[B]

  def foldLeft[U >: A, B](z: B)(f: (B, U) => B): B

  def foldRight[U >: A, B](z: B)(f: (U, B) => B): B

  def reverse: List[A]

  def map[B](f: A => B): List[B]

  def flatMap[B](f: A => List[B]): List[B]

  def filter(f: A => Boolean): List[A]

  def foreach[B](f: A => B): Unit

  def zip[B](bs: List[B]): List[(A, B)]

  def zipWithIndex: List[(A, Int)]

  def take(n: Int): List[A]

  def takeWhile(p: A => Boolean): List[A]
}

case object Nil extends List[Nothing] {
  override def head(): Nothing = throw new UnsupportedOperationException("head of empty list")

  override def tail(): Nothing = throw new UnsupportedOperationException("tail of empty list")

  override def init(): List[Nothing] = throw new UnsupportedOperationException("init of empty list")

  override def ::[B >: Nothing](x: B): List[B] = Cons(x, this)

  override def :::[B >: Nothing](nxs: List[B]): List[B] = nxs

  override def toString: String = ""

  override def isEmpty: Boolean = true

  override def length: Int = 0

  override def setHead[B >: Nothing](newHead: B): List[B] = Cons(newHead, Nil)

  override def drop(n: Int): List[Nothing] = this

  override def dropWhile[B >: Nothing](f: (B) => Boolean): List[B] = this

  override def foldLeft[U >: Nothing, B](z: B)(f: (B, U) => B): B = z

  override def foldRight[U >: Nothing, B](z: B)(f: (U, B) => B): B = z

  override def reverse: List[Nothing] = this

  override def map[B](f: (Nothing) => B): List[B] = this

  override def flatMap[B](f: (Nothing) => List[B]): List[B] = this

  override def filter(f: (Nothing) => Boolean): List[Nothing] = this

  override def foreach[B](f: (Nothing) => B): Unit = Unit.asInstanceOf[B]

  override def zip[B](bs: List[B]): List[(Nothing, B)] = this

  override def zipWithIndex: List[(Nothing, Int)] = this

  override def take(n: Int): List[Nothing] = this

  override def takeWhile(p: (Nothing) => Boolean): List[Nothing] = this
}


case class Cons[+A](x: A, xs: List[A]) extends List[A] {
  override def head(): A = this.x

  override def tail(): List[A] = this.xs

  override def init(): List[A] = xs match {
    case Nil => Nil
    case Cons(rx, Nil) => Cons(x, Nil)
    case _ => Cons(x, xs.init())
  }

  override def ::[B >: A](x: B): List[B] = Cons(x, this)

  //  override def :::[B >: A](nxs: List[B]): List[B] = xs match {
  //    case Nil => Cons(x, nxs)
  //    case Cons(rx, rs: List[B]) => Cons(x, Cons(rx, rs ::: nxs))
  //  }
  override def :::[B >: A](nxs: List[B]): List[B] = nxs.foldRight(this.asInstanceOf[List[B]])(Cons(_, _))

  override def toString: String = x.toString + ", " + xs.toString

  override def isEmpty: Boolean = false

  //  override def length: Int = {
  //    @tailrec
  //    def loop(len: Int, as: List[A]): Int = as match {
  //      case Nil => len
  //      case Cons(a, rs) => loop(len + 1, rs)
  //    }
  //    loop(0, this)
  //  }

  override def length: Int = foldLeft(0)((n, _) => n + 1)

  override def setHead[B >: A](newHead: B): List[B] = Cons(newHead, this.xs)

  override def drop(n: Int): List[A] = {
    @tailrec
    def loop(i: Int, l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(a, rs) if i > 0 => loop(i - 1, rs)
      case _ => l
    }
    loop(n, this)
  }

  override def dropWhile[B >: A](f: (B) => Boolean): List[B] = {
    @tailrec
    def loop[U >: A](l: List[U])(f: (U) => Boolean): List[U] = l match {
      case Nil => Nil
      case Cons(a, rs) if f(a) => loop(rs)(f)
      case _ => l
    }
    loop(this)(f)
  }

  //override def foldLeft[U >: A, B](z: B)(f: (B, U) => B): B = xs.foldLeft(f(z, x))(f)

  override def foldLeft[U >: A, B](z: B)(f: (B, U) => B): B = {
    @tailrec
    def loop(as: List[U], z: B)(f: (B, U) => B): B = as match {
      case Nil => z
      case Cons(a, rs) => loop(rs, f(z, a))(f)
    }
    loop(this, z)(f)
  }

  override def reverse: List[A] = foldLeft(List[A]())(_.::(_))

  //override def foldRight[U >: A, B](z: B)(f: (U, B) => B): B = f(x, xs.foldRight(z)(f))

  override def foldRight[U >: A, B](z: B)(f: (U, B) => B): B = this.reverse.foldLeft(z)((b, u) => f(u, b)) //less performance but stack safe

  override def map[B](f: (A) => B): List[B] = this.foldRight(List[B]())((element, list) => f(element) :: list)

  override def flatMap[B](f: (A) => List[B]): List[B] = this.foldLeft(List[B]())((list, element) => list ::: f(element))

  override def filter(f: (A) => Boolean): List[A] = this.flatMap(x => if (f(x)) List(x) else Nil)

  override def foreach[B](f: (A) => B): Unit = this.foldLeft(Unit.asInstanceOf[B])((_, x) => f(x))

  override def zip[B](bs: List[B]): List[(A, B)] = {
    @tailrec
    def loop(zipped: List[(A, B)], ps: (List[A], List[B])): List[(A, B)] = ps match {
      case (Nil, Cons(_, _)) | (Cons(_, _), Nil) | (Nil, Nil) => zipped
      case (Cons(a, ars), Cons(b, brs)) => loop((a, b) :: zipped, (ars, brs))
    }
    loop(Nil, (this, bs)).reverse
  }

  override def zipWithIndex: List[(A, Int)] = {
    @tailrec
    def loop(zipped: List[(A, Int)], i: Int, xs: List[A]): List[(A, Int)] = xs match {
      case Nil => zipped
      case Cons(a, rs) => loop((a, i) :: zipped, i + 1, rs)
    }
    loop(Nil, 0, this).reverse
  }

  override def take(n: Int): List[A] = {
    @tailrec
    def loop(i: Int, taken: List[A], as: List[A]): List[A] = as match {
      case Nil => taken
      case Cons(a, rs) if i > 0 => loop(i - 1, a :: taken, rs)
      case _ => taken
    }
    loop(n, Nil, this).reverse
  }

  override def takeWhile(p: (A) => Boolean): List[A] = {
    @tailrec
    def loop(taken: List[A], as: List[A])(p: A => Boolean): List[A] = as match {
      case Nil => taken
      case Cons(a, rs) if p(a) => loop(a :: taken, rs)(p)
      case _ => taken
    }
    loop(Nil, this)(p).reverse
  }
}

object List {
  def sum(ints: List[Int]): Int = ints.foldLeft(0)(_ + _)

  def product(ds: List[Double]): Double = ds.foldLeft(1.0D)(_ * _)

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(x, rs) => f(x, foldRight(rs, z)(f)) //no tailrec, quire a failure
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}


class ListSample {
  @Test
  def testCaseMatching(): Unit = {
    val v = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    assert(3 == v)
  }

  @Test(expected = classOf[UnsupportedOperationException])
  def testTail(): Unit = {
    assert(List(2, 3) == List(1, 2, 3).tail())
    Nil.tail()
  }

  @Test
  def testSetHead(): Unit = {
    assert(List(0) == Nil.setHead(0))
    assert(List(0, 2, 3) == List(1, 2, 3).setHead(0))
  }

  @Test
  def testLength(): Unit = {
    assert(0 == Nil.length)
    assert(1 == List(0).length)
    assert(3 == List(0, 1, 2).length)
  }

  @Test
  def testDrop(): Unit = {
    assert(List(1, 2, 3) == List(1, 2, 3).drop(0))
    assert(List(2, 3) == List(1, 2, 3).drop(1))
    assert(Nil == List(1, 2, 3).drop(9))
    assert(Nil == Nil.drop(1))
  }

  @Test
  def testDropWhile(): Unit = {
    assert(List(3) == List(1, 2, 3).dropWhile(_ < 3))
    assert(Nil == List(1, 2, 3).dropWhile(_ > 0))
    assert(Nil == Nil.dropWhile[String](_.nonEmpty))
  }


  @Test
  def testCons(): Unit = {
    assert(List(1, 2, 3) == 1 :: 2 :: 3 :: Nil)
  }

  @Test
  def testAppend(): Unit = {
    assert(List(1, 2, 3) == Nil ::: List(1, 2, 3))
    assert(List(1, 2, 3) == List(1, 2, 3) ::: Nil)
    assert(List(4, 6, 6, 1, 2, 3) == List(4, 6, 6) ::: List(1, 2, 3))
  }

  @Test(expected = classOf[UnsupportedOperationException])
  def testInit(): Unit = {
    assert(List(1, 2) == List(1, 2, 3).init())
    assert(List(1) == List(1, 2).init())
    assert(Nil == List(1).init())
    assert(Nil == Nil.init())
  }

  @Test
  def testFoldLeft(): Unit = {
    assert(1 == Nil.foldLeft[Int,Int](1)(_+_))
    assert(6 == List(1, 2, 3).foldLeft(1)(_ * _))
    assert(List(3, 2, 1) == List(1, 2, 3).foldLeft(Nil: List[Int])((lst, itm) => Cons(itm, lst)))
  }

  @Test
  def testFoldRight(): Unit = {
    assert(0 == Nil.foldRight[Int,Int](0)(_+_))
    assert(6 == List(1, 2, 3).foldRight(0)(_ + _))
    assert(List(1, 2, 3) == List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)))
    assert(List(1, 2, 3, 7, 8, 9) == List(1, 2, 3).foldRight(List(7, 8, 9))(Cons(_, _)))
  }

  @Test
  def testAddingOne(): Unit = {
    assert(List(2, 3, 4) == List(1, 2, 3).reverse.foldLeft(List[Int]())((list, element) => (element + 1) :: list))
  }

  @Test
  def testDblToString(): Unit = {
    assert(List("0.001", "3.1415", "1.11926") == List(0.001D, 3.1415D, 1.11926D).reverse.foldLeft(List[String]())((list, element) => element.toString :: list))
  }

  @Test
  def testMap(): Unit = {
    assert(List("1", "2", "3") == List(1, 2, 3).map(_.toString))
  }

  @Test
  def testFlapMap(): Unit = {
    assert(List(1, 1, 2, 2, 3, 3) == List(1, 2, 3).flatMap(element => List(element, element)))
  }

  @Test
  def testFilter(): Unit = {
    assert(List(1, 2, 3) == List(-1, 0, 1, 2, 3).flatMap(x => if (x > 0) List(x) else List[Int]()))
    assert(List(2, 4, 6) == List(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0))
  }

  @Test
  def testForeach(): Unit = {
    List(1, 2, 3).foreach(print)
  }

  @Test
  def testZip(): Unit = {
    assert(Nil == Nil.zip(Nil))
    assert(Nil == Nil.zip(List(1, 2, 3)))
    assert(Nil == List(1, 2, 3).zip(Nil))
    assert(List((1, "One"), (2, "Two")) == List(1, 2, 3).zip(List("One", "Two")))
    assert(List((1, "One"), (2, "Two")) == List(1, 2).zip(List("One", "Two", "Three")))
    assert(List((1, "One"), (2, "Two"), (3, "Three")) == List(1, 2, 3).zip(List("One", "Two", "Three")))
  }

  @Test
  def testZipWithIndex(): Unit = {
    assert(Nil == Nil.zipWithIndex)
    assert(List((1, 0), (2, 1), (3, 2)) == List(1, 2, 3).zipWithIndex)
  }

  @Test
  def testTake(): Unit = {
    assert(Nil == Nil.take(0))
    assert(Nil == List(1, 2).take(-1))
    assert(Nil == List(1, 2).take(0))
    assert(List(1) == List(1, 2).take(1))
    assert(List(1, 2) == List(1, 2).take(2))
    assert(List(1, 2) == List(1, 2).take(3))
  }

  @Test
  def testTakeWhile(): Unit = {
    assert(Nil == Nil.takeWhile(_.isInstanceOf[Unit]))
    assert(Nil == List(1, 2, 3).takeWhile(_ < 1))
    assert(List(1) == List(1, 2, 3).takeWhile(_ < 2))
    assert(List(1, 2) == List(1, 2, 3).takeWhile(_ < 3))
  }


}
