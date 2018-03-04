package fp.intro

import org.junit.Test

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
    println(Lst(1, 2) ::: Lst(3, 4) ::: Lst(5, 6))
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
    //assert(1 == Nls.foldLeft[Int](1)(_ + _.asInstanceOf[Int]))
    assert(6 == Lst(1, 2, 3).foldLeft(1)(_ * _))
    assert(Lst(3, 2, 1) == Lst(1, 2, 3).foldLeft(Nls: Lst[Int])((lst, itm) => Cnl(itm, lst)))
  }

  @Test
  def testFoldRight(): Unit = {
    //assert(0 == Nls.foldRight[Int](0)(_.asInstanceOf[Int] + _))
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
    assert(Lst(1, 2, 3, 4) == Lst(Lst(1, 2), Lst(3, 4)).flatMap(identity))
    println(Lst(Lst(1, 2), Lst(3, 4)).flatMap(identity))
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
    //assert(Nls == Nls.takeWhile(_.isInstanceOf[Unit]))
    assert(Nls == Lst(1, 2, 3).takeWhile(_ < 1))
    assert(Lst(1) == Lst(1, 2, 3).takeWhile(_ < 2))
    assert(Lst(1, 2) == Lst(1, 2, 3).takeWhile(_ < 3))
  }

  @Test
  def testFind(): Unit = {
    //assert(Nls.find(_.equals(0)).isEmpty)
    assert(Lst(1, 2, 3).find(_ < 0).isEmpty)
    assert(Lst(1, 2, 3).find(_ > 2).isDefined)
  }

  @Test
  def testExists(): Unit = {
    //assert(!Nls.exists(_.equals(0)))
    assert(!Lst(1, 2, 3).exists(_ < 0))
    assert(Lst(1, 2, 3).exists(_ > 2))
  }

  @Test
  def testForall(): Unit = {
    //assert(!Nls.forall(_.equals(0)))
    assert(!Lst(1, 2, 3).forall(_ == 0))
    assert(Lst(1, 2, 3).forall(_ > 0))
    assert(!Lst(1, 2, 3).forall(_ < 2))
  }

  @Test
  def testContains(): Unit = {
    assert(!Nls.contains(Nls))
    assert(!Nls.contains(Lst(1)))
    assert(Lst(1, 2).contains(Nls))
    assert(Lst(1, 2).contains(Lst(1)))
    assert(Lst(1, 2).contains(Lst(1, 2)))
    assert(!Lst(1, 2).contains(Lst(1, 2, 3)))
  }

}
