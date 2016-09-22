package basic.collects.lists

import org.junit.{Assert, Test}

import scala.annotation.tailrec

class ListSample {
  @Test
  def listSample(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertTrue(list.length == 3)
  }

  @Test
  def listConcatenation(): Unit = {
    val first = List(0, 1, 2)
    val second = List(3, 4, 5)
    val combine = first ::: second
    Assert.assertTrue(combine.equals(List(0, 1, 2, 3, 4, 5)))
    Assert.assertTrue(combine == List(0, 1, 2, 3, 4, 5))
  }

  @Test
  def emptyList(): Unit = {
    val emptyList = List()
    val nilList = Nil
    Assert.assertTrue(emptyList.isEmpty)
    Assert.assertTrue(nilList.isEmpty)
  }

  @Test
  def listConsElement(): Unit = {
    val list = List()
    // :: is a method of List
    val one = 1 :: list
    Assert.assertEquals(1, one.length)
  }

  @Test
  def nilConsElement(): Unit = {
    val list = 1 :: 2 :: 3 :: 4 :: Nil
    Assert.assertEquals(4, list.length)
  }

  @Test
  def element(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(2, list(1))
    Assert.assertEquals(2, list.apply(1))
  }

  @Test
  def count(): Unit = {
    val list = List(1, 1, 2, 2, 3, 3, 4, 4)
    Assert.assertEquals(4, list.count(item => {
      item % 2 == 0
    }))
  }

  @Test
  def dropElementsAtHead(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(3), list.drop(2))
  }

  @Test
  def dropElementsAtTail(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(1), list.dropRight(2))
  }

  @Test
  def dropWhile(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(2, 3), list.dropWhile(item => {
      item % 2 == 1
    }))
  }

  @Test
  def elementFilter(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(1, 3), list.filter(item => {
      item % 2 == 1
    }))
  }

  @Test
  def elementExists(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertTrue(list.exists(item => {
      item % 2 == 1
    }))
  }

  @Test
  def elementForAll(): Unit = {
    val list = List(2, 4, 6, 8)
    Assert.assertTrue(list.exists(item => {
      item % 2 == 0
    }))
  }

  @Test
  def foreachElement(): Unit = {
    val list = List(1, 2, 3)
    list.foreach(print)
  }

  @Test
  def headElement(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(1, list.head)
  }

  @Test
  def lastElement(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(3, list.last)
  }

  @Test
  def initElements(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(1, 2), list.init)
  }

  @Test
  def tailElements(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List(2, 3), list.tail)
  }

  @Test
  def testEmpty(): Unit = {
    Assert.assertTrue(Nil.isEmpty)
  }

  @Test
  def mapElement(): Unit = {
    val list = List(1, 2, 3)
    Assert.assertEquals(List("1", "2", "3"), list.map(item => item.toString))
  }

  @Test
  def mapConserve(): Unit = {
    val l = List("foo", "bar", "baz")
    // List.mapConserve(), A <: AnyRef, A must be a subtype of anyRef
    // Int is a subType of anyVal, wont work for Int
    Assert.assertEquals(List("FOO", "BAR", "BAZ"), l.mapConserve(_.toUpperCase))
  }

  @Test
  def sortElements(): Unit = {
    val words = "The quick brown fox jumped over the lazy dog".split(' ')
    val expected = Array("The", "dog", "fox", "the", "lazy", "over", "brown", "quick", "jumped").mkString(",")
    Assert.assertEquals(expected, words.sortBy(x => (x.length, x.head)).mkString(","))
    val list = List(1, 2, 3)
    Assert.assertEquals(List(1, 2, 3), list.sortBy(identity))
  }

  @Test
  def sortWith(): Unit = {
    val expected = List("Bob", "John", "Steve", "Tom")
    val sorted = List("Steve", "Tom", "John", "Bob").sortWith((left, right) => left.compareTo(right) < 0)
    Assert.assertTrue(expected == sorted)
  }


  def maxSubList(xs: List[Int]): List[Int] = {
    @tailrec
    def loop(mxsq: List[Int], tmpsub: List[Int], xs: List[Int]): List[Int] = xs match {
      case x :: rs =>
        val sub = x :: tmpsub
        loop(if (sub.sum > mxsq.sum) sub else mxsq, if (sub.sum > 0) sub else Nil, rs)
      case Nil => mxsq
    }
    loop(List.empty[Int], List.empty[Int], xs).reverse
  }

  @Test
  def testMaxSubList(): Unit = {
    val l1 = maxSubList(List(-2, 1, -3, 4, -1, 2, 1, -5, 4))
    println(s"l1=$l1, l1.sum=${l1.sum}")
    val l2 = maxSubList(List(9, -2, 1, -3, 4, -1, 2, 1, -5, 11, 4))
    println(s"l2=$l2, l1.sum=${l2.sum}")
    val l3 = maxSubList(List(-2, 1, -3))
    println(s"l1=$l3, l1.sum=${l3.sum}")
  }
}
