package fp.intro

import org.junit.Test

import scala.annotation.tailrec

sealed trait Lst[+A] {
  def head: A

  def tail: Lst[A]

  def headOption: Option[A]

  def init: Lst[A]

  def ::[B >: A](x: B): Lst[B]

  def :::[B >: A](nxs: Lst[B]): Lst[B]

  def isEmpty: Boolean

  def length: Int

  def drop(n: Int): Lst[A]

  def dropWhile[B >: A](f: B => Boolean): Lst[B]

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def foldRight[B](z: B)(f: (A, B) => B): B

  def reverse: Lst[A]

  def map[B](f: A => B): Lst[B]

  def flatMap[B](f: A => Lst[B]): Lst[B]

  def filter(f: A => Boolean): Lst[A]

  def foreach[B](f: A => B): Unit

  def zip[B](bs: Lst[B]): Lst[(A, B)]

  def zipWithIndex: Lst[(A, Int)]

  def take(n: Int): Lst[A]

  def takeWhile(p: A => Boolean): Lst[A]

  def find(p: A => Boolean): Option[A]

  def exists(p: A => Boolean): Boolean

  def forall(p: A => Boolean): Boolean

  def contains[U >: A](sub: Lst[U]): Boolean
}

case object Nls extends Lst[Nothing] {
  override def head: Nothing = throw new UnsupportedOperationException("head of empty list")

  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")

  override def headOption: Option[Nothing] = None

  override def init: Lst[Nothing] = this

  override def ::[B >: Nothing](x: B): Lst[B] = Cnl(x, this)

  override def :::[B >: Nothing](nxs: Lst[B]): Lst[B] = nxs

  override def toString: String = "Nil"

  override def isEmpty: Boolean = true

  override def length: Int = 0

  override def drop(n: Int): Lst[Nothing] = this

  override def dropWhile[B >: Nothing](f: (B) => Boolean): Lst[B] = this

  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def reverse: Lst[Nothing] = this

  override def map[B](f: (Nothing) => B): Lst[B] = this

  override def flatMap[B](f: (Nothing) => Lst[B]): Lst[B] = this

  override def filter(f: (Nothing) => Boolean): Lst[Nothing] = this

  override def foreach[B](f: (Nothing) => B): Unit = Unit.asInstanceOf[B]

  override def zip[B](bs: Lst[B]): Lst[(Nothing, B)] = this

  override def zipWithIndex: Lst[(Nothing, Int)] = this

  override def take(n: Int): Lst[Nothing] = this

  override def takeWhile(p: (Nothing) => Boolean): Lst[Nothing] = this

  override def find(p: (Nothing) => Boolean): Option[Nothing] = None

  override def exists(p: (Nothing) => Boolean): Boolean = false

  override def forall(p: (Nothing) => Boolean): Boolean = false

  override def contains[U >: Nothing](sub: Lst[U]): Boolean = if (Nls == sub) true else false
}


case class Cnl[+A](x: A, xs: Lst[A]) extends Lst[A] {
  override def head: A = this.x

  override def tail: Lst[A] = this.xs

  override def headOption: Option[A] = Some(x)

  override def init: Lst[A] = {
    @tailrec
    def loop(il: Lst[A], lst: Lst[A]): Lst[A] = lst match {
      case Nls => Nls
      case Cnl(a, rs) if rs == Nls => il
      case Cnl(a, rs) if rs != Nls => loop(a :: il, rs)
    }
    loop(Nls, this).reverse
  }

  override def ::[B >: A](x: B): Lst[B] = Cnl(x, this)

  //  override def :::[B >: A](nxs: List[B]): List[B] = xs match {
  //    case Nls => Cnl(x, nxs)
  //    case Cnl(rx, rs: List[B]) => Cnl(x, Cnl(rx, rs ::: nxs))
  //  }
  override def :::[B >: A](nxs: Lst[B]): Lst[B] = nxs.foldRight(this.asInstanceOf[Lst[B]])(Cnl(_, _))

  override def toString: String = {
    @tailrec
    def loop(ts: String, lst: Lst[A]): String = lst match {
      case Nls => s"$ts::${Nls.toString}"
      case Cnl(a, rs) => loop(s"$ts::${a.toString}", rs)
    }
    loop("", this).drop(2)
  }

  override def isEmpty: Boolean = false

  //  override def length: Int = {
  //    @tailrec
  //    def loop(len: Int, as: List[A]): Int = as match {
  //      case Nls => len
  //      case Cnl(a, rs) => loop(len + 1, rs)
  //    }
  //    loop(0, this)
  //  }

  override def length: Int = foldLeft(0)((n, _) => n + 1)

  override def drop(n: Int): Lst[A] = {
    @tailrec
    def loop(i: Int, l: Lst[A]): Lst[A] = l match {
      case Cnl(a, rs) if i > 0 => loop(i - 1, rs)
      case _ => l
    }
    loop(n, this)
  }

  override def dropWhile[B >: A](p: (B) => Boolean): Lst[B] = {
    @tailrec
    def loop[U >: A](l: Lst[U])(f: (U) => Boolean): Lst[U] = l match {
      case Cnl(a, rs) if f(a) => loop(rs)(f)
      case _ => l
    }
    loop(this)(p)
  }

  //override def foldLeft[U >: A, B](z: B)(f: (B, U) => B): B = xs.foldLeft(f(z, x))(f)

  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: Lst[A], z: B)(f: (B, A) => B): B = as match {
      case Nls => z
      case Cnl(a, rs) => loop(rs, f(z, a))(f)
    }
    loop(this, z)(f)
  }

  override def reverse: Lst[A] = foldLeft(Lst[A]())(_.::(_))

  //override def foldRight[U >: A, B](z: B)(f: (U, B) => B): B = f(x, xs.foldRight(z)(f))

  override def foldRight[B](z: B)(f: (A, B) => B): B = this.reverse.foldLeft(z)((b, u) => f(u, b)) //less performance but stack safe

  override def map[B](f: (A) => B): Lst[B] = this.foldRight(Lst[B]())((element, list) => f(element) :: list)

  override def flatMap[B](f: (A) => Lst[B]): Lst[B] = this.foldLeft(Lst[B]())((list, element) => list ::: f(element))

  override def filter(p: (A) => Boolean): Lst[A] = this.flatMap(x => if (p(x)) Lst(x) else Nls)

  override def foreach[B](f: (A) => B): Unit = this.foldLeft(Unit.asInstanceOf[B])((_, x) => f(x))

  override def zip[B](bs: Lst[B]): Lst[(A, B)] = {
    @tailrec
    def loop(zipped: Lst[(A, B)], as: Lst[A], bs: Lst[B]): Lst[(A, B)] = (as, bs) match {
      case (Cnl(a, ars), Cnl(b, brs)) => loop((a, b) :: zipped, ars, brs)
      case _ => zipped
    }
    loop(Nls, this, bs).reverse
  }

  override def zipWithIndex: Lst[(A, Int)] = {
    @tailrec
    def loop(zipped: Lst[(A, Int)], i: Int, xs: Lst[A]): Lst[(A, Int)] = xs match {
      case Nls => zipped
      case Cnl(a, rs) => loop((a, i) :: zipped, i + 1, rs)
    }
    loop(Nls, 0, this).reverse
  }

  override def take(n: Int): Lst[A] = {
    @tailrec
    def loop(i: Int, taken: Lst[A], as: Lst[A]): Lst[A] = as match {
      case Cnl(a, rs) if i > 0 => loop(i - 1, a :: taken, rs)
      case _ => taken
    }
    loop(n, Nls, this).reverse
  }

  override def takeWhile(p: (A) => Boolean): Lst[A] = {
    @tailrec
    def loop(taken: Lst[A], as: Lst[A])(p: A => Boolean): Lst[A] = as match {
      case Cnl(a, rs) if p(a) => loop(a :: taken, rs)(p)
      case _ => taken
    }
    loop(Nls, this)(p).reverse
  }

  override def find(p: (A) => Boolean): Option[A] = {
    @tailrec
    def loop(as: Lst[A])(p: A => Boolean): Option[A] = as match {
      case Cnl(a, rs) if p(a) => Some(a)
      case Cnl(a, rs) if !p(a) => loop(rs)(p)
      case _ => None
    }
    loop(this)(p)
  }

  override def exists(p: (A) => Boolean): Boolean = this.find(p).nonEmpty

  override def forall(p: (A) => Boolean): Boolean = {
    @tailrec
    def loop(as: Lst[A])(p: A => Boolean): Boolean = as match {
      case Cnl(a, _) if !p(a) => false
      case Cnl(a, Nls) if p(a) => true
      case Cnl(a, rs) if p(a) => loop(rs)(p)
      case _ => false
    }
    loop(this)(p)
  }

  override def contains[U >: A](sub: Lst[U]): Boolean = {
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


class ListSample {
  @Test
  def testToString(): Unit = {
    assert("1::2::3::Nil" == Lst(1, 2, 3).toString)
  }

  @Test
  def testCaseMatching(): Unit = {
    val v = Lst(1, 2, 3, 4, 5) match {
      case Cnl(x, Cnl(2, Cnl(4, _))) => x
      case Nls => 42
      case Cnl(x, Cnl(y, Cnl(3, Cnl(4, _)))) => x + y
      case Cnl(h, t) => h + Lst.sum(t)
      case _ => 101
    }

    assert(3 == v)
  }

  @Test(expected = classOf[UnsupportedOperationException])
  def testTail(): Unit = {
    assert(Lst(2, 3) == Lst(1, 2, 3).tail)
    Nls.tail
  }

  @Test
  def testLength(): Unit = {
    assert(0 == Nls.length)
    assert(1 == Lst(0).length)
    assert(3 == Lst(0, 1, 2).length)
  }

  @Test
  def testDrop(): Unit = {
    assert(Lst(1, 2, 3) == Lst(1, 2, 3).drop(0))
    assert(Lst(2, 3) == Lst(1, 2, 3).drop(1))
    assert(Nls == Lst(1, 2, 3).drop(9))
    assert(Nls == Nls.drop(1))
  }

  @Test
  def testDropWhile(): Unit = {
    assert(Lst(3) == Lst(1, 2, 3).dropWhile(_ < 3))
    assert(Nls == Lst(1, 2, 3).dropWhile(_ > 0))
    assert(Nls == Nls.dropWhile[String](_.nonEmpty))
  }


  @Test
  def testCnl(): Unit = {
    assert(Lst(1, 2, 3) == 1 :: 2 :: 3 :: Nls)
  }

  @Test
  def testAppend(): Unit = {
    assert(Lst(1, 2, 3) == Nls ::: Lst(1, 2, 3))
    assert(Lst(1, 2, 3) == Lst(1, 2, 3) ::: Nls)
    assert(Lst(4, 6, 6, 1, 2, 3) == Lst(4, 6, 6) ::: Lst(1, 2, 3))
  }

  @Test
  def testInit(): Unit = {
    assert(Lst(1, 2) == Lst(1, 2, 3).init)
    assert(Lst(1) == Lst(1, 2).init)
    assert(Nls == Lst(1).init)
    assert(Nls == Nls.init)
  }

  @Test
  def testFoldLeft(): Unit = {
    assert(1 == Nls.foldLeft[Int](1)(_+_.asInstanceOf[Int]))
    assert(6 == Lst(1, 2, 3).foldLeft(1)(_ * _))
    assert(Lst(3, 2, 1) == Lst(1, 2, 3).foldLeft(Nls: Lst[Int])((lst, itm) => Cnl(itm, lst)))
  }

  @Test
  def testFoldRight(): Unit = {
    assert(0 == Nls.foldRight[Int](0)(_.asInstanceOf[Int]+_))
    assert(6 == Lst(1, 2, 3).foldRight(0)(_ + _))
    assert(Lst(1, 2, 3) == Lst(1, 2, 3).foldRight(Nls: Lst[Int])(Cnl(_, _)))
    assert(Lst(1, 2, 3, 7, 8, 9) == Lst(1, 2, 3).foldRight(Lst(7, 8, 9))(Cnl(_, _)))
  }

  @Test
  def testAddingOne(): Unit = {
    assert(Lst(2, 3, 4) == Lst(1, 2, 3).reverse.foldLeft(Lst[Int]())((list, element) => (element + 1) :: list))
  }

  @Test
  def testDblToString(): Unit = {
    assert(Lst("0.001", "3.1415", "1.11926") == Lst(0.001D, 3.1415D, 1.11926D).reverse.foldLeft(Lst[String]())((list, element) => element.toString :: list))
  }

  @Test
  def testMap(): Unit = {
    assert(Lst("1", "2", "3") == Lst(1, 2, 3).map(_.toString))
  }

  @Test
  def testFlapMap(): Unit = {
    assert(Lst(1, 1, 2, 2, 3, 3) == Lst(1, 2, 3).flatMap(element => Lst(element, element)))
  }

  @Test
  def testFilter(): Unit = {
    assert(Lst(1, 2, 3) == Lst(-1, 0, 1, 2, 3).flatMap(x => if (x > 0) Lst(x) else Lst[Int]()))
    assert(Lst(2, 4, 6) == Lst(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0))
  }

  @Test
  def testForeach(): Unit = {
    Lst(1, 2, 3).foreach(_.ensuring(_ > 0))
  }

  @Test
  def testZip(): Unit = {
    assert(Nls == Nls.zip(Nls))
    assert(Nls == Nls.zip(Lst(1, 2, 3)))
    assert(Nls == Lst(1, 2, 3).zip(Nls))
    assert(Lst((1, "One"), (2, "Two")) == Lst(1, 2, 3).zip(Lst("One", "Two")))
    assert(Lst((1, "One"), (2, "Two")) == Lst(1, 2).zip(Lst("One", "Two", "Three")))
    assert(Lst((1, "One"), (2, "Two"), (3, "Three")) == Lst(1, 2, 3).zip(Lst("One", "Two", "Three")))
  }

  @Test
  def testZipWithIndex(): Unit = {
    assert(Nls == Nls.zipWithIndex)
    assert(Lst((1, 0), (2, 1), (3, 2)) == Lst(1, 2, 3).zipWithIndex)
  }

  @Test
  def testTake(): Unit = {
    assert(Nls == Nls.take(0))
    assert(Nls == Lst(1, 2).take(-1))
    assert(Nls == Lst(1, 2).take(0))
    assert(Lst(1) == Lst(1, 2).take(1))
    assert(Lst(1, 2) == Lst(1, 2).take(2))
    assert(Lst(1, 2) == Lst(1, 2).take(3))
  }

  @Test
  def testTakeWhile(): Unit = {
    assert(Nls == Nls.takeWhile(_.isInstanceOf[Unit]))
    assert(Nls == Lst(1, 2, 3).takeWhile(_ < 1))
    assert(Lst(1) == Lst(1, 2, 3).takeWhile(_ < 2))
    assert(Lst(1, 2) == Lst(1, 2, 3).takeWhile(_ < 3))
  }

  @Test
  def testFind(): Unit = {
    assert(Nls.find(_.equals(0)).isEmpty)
    assert(Lst(1, 2, 3).find(_ < 0).isEmpty)
    assert(Lst(1, 2, 3).find(_ > 2).isDefined)
  }

  @Test
  def testExists(): Unit = {
    assert(!Nls.exists(_.equals(0)))
    assert(!Lst(1, 2, 3).exists(_ < 0))
    assert(Lst(1, 2, 3).exists(_ > 2))
  }

  @Test
  def testForall(): Unit = {
    assert(!Nls.forall(_.equals(0)))
    assert(!Lst(1, 2, 3).forall(_ == 0))
    assert(Lst(1, 2, 3).forall(_ > 0))
    assert(!Lst(1, 2, 3).forall(_ < 2))
  }

  @Test
  def testContains(): Unit = {
    assert(Nls.contains(Nls))
    assert(!Nls.contains(Lst(1)))
    assert(Lst(1, 2).contains(Nls))
    assert(Lst(1, 2).contains(Lst(1)))
    assert(Lst(1, 2).contains(Lst(1, 2)))
    assert(!Lst(1, 2).contains(Lst(1, 2, 3)))
  }

}
