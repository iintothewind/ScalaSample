package basic.collects.lists

import org.junit.Test

import scala.language.postfixOps

class HighOrderOperations {

  @Test
  def testMap(): Unit = {
    println(List("the", "quick", "brown", "fox").map(_.length).mkString(","))
    println(List("the", "quick", "brown", "fox").map(_.reverse).mkString(","))
  }

  @Test
  def flatMap(): Unit = {
    println(List("the", "quick", "brown", "fox").flatMap(_.toList).mkString(","))
    println(List("the", "quick", "brown", "fox").flatMap(identity(_)).mkString(","))
    println(List("the", "quick", "brown", "fox").flatMap(s => List(s)).flatMap(identity(_)).mkString(","))
  }

  @Test
  def filter(): Unit = {
    assert(List(2, 4) == List(1, 2, 3, 4, 5).filter(_ % 2 == 0))
  }

  @Test
  def collect(): Unit = {
    val list = List("the", "quick", "brown", "fox", "jump", "over", "the", "lazy", "dog")
    val map = list.collect({
      case word if word.length > 4 => (word, word.length)
    })
    assert(Map("quick" -> 5, "brown" -> 5) == map)
  }

  @Test
  def parMap(): Unit = {
    def doIt(x: Int): Int = {
      println(s"calc $x in ${Thread.currentThread().getName}")
      x * 2
    }
    // import scala.collection.parallel.CollectionConverters._
    // scala-parallel-collections should be imported before par can be used
    // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0).par.map(doIt);
  }

  @Test
  def groupBy(): Unit = {
    assert(Map(true -> List(2, 4), false -> List(1, 3, 5)) == List(1, 2, 3, 4, 5).groupBy(_ % 2 == 0))
  }

  @Test
  def partition(): Unit = {
    assert((List(2, 4), List(1, 3, 5)) == List(1, 2, 3, 4, 5).partition(_ % 2 == 0))
  }

  @Test
  def find(): Unit = {
    assert(2 == List(1, 2, 3, 4, 5).find(_ % 2 == 0).get)
  }

  @Test
  def takeWhile(): Unit = {
    assert(List(1, 2) == List(1, 2, 3, 4, 5).takeWhile(_ < 3))
  }

  @Test
  def dropWhile(): Unit = {
    assert(List(4, 5) == List(1, 2, 3, 4, 5).dropWhile(_ <= 3))
  }

  @Test
  def sliding(): Unit = {
    assert(List(List(1, 2), List(2, 3)) == List(1, 2, 3, 5, 7, 9).sliding(2, 1).filter(e => e(1) - e.head <= 1).toList)
  }

  @Test
  def span(): Unit = {
    assert((List(1, 2), List(3, 4, 5)) == List(1, 2, 3, 4, 5).span(_ < 3))
  }

  @Test
  def forall(): Unit = {
    assert(List(1, 2, 3).forall(_ > 0))
  }

  @Test
  def exists(): Unit = {
    assert(List(-1, 0, 1).exists(_ > 0))
  }

  /**
    * deprecated
    */
  @Test
  def leftFolding(): Unit = {
    // left folding :/ is an operator of TraversableOnce,
    // it takes a start value in the first parameter entrance,
    // and a binary op closure in the second parameter entrance
    // the result of left folding is the nested result of operation between start value and the elements of list from start to end.
    //    assert(List(1, 2, 3).sum == (0 /: List(1, 2, 3)) (_ + _))
    //    assert((0 /: List(1, 2, 3)) (_ + _) == List(1, 2, 3).foldLeft(0)(_ + _))
    //    assert(Seq(1, 2, 3).foldLeft(0)(_ + _) == Seq(1, 2, 3).reverse.foldRight(0)(_ + _))
  }

  /**
    * deprecated
    */
  @Test
  def rightFolding(): Unit = {
    //    assert(List(1, 2, 3).product == (List(1, 2, 3) :\ 1) (_ * _))
    //    assert((List(1, 2, 3) :\ 1) (_ * _) == List(1, 2, 3).foldRight(1)(_ * _))
  }

  /**
    * deprecated
    */
  @Test
  def reverseByLeftFolding(): Unit = {
    //    def reverse(list: List[Int]) = (List.empty[Int] /: list) ((foldingList, element) => element :: foldingList)
    //
    //    assert(List(3, 2, 1) == reverse(List(1, 2, 3)))
  }

  /**
    * deprecated
    */
  @Test
  def reverseByRightFolding(): Unit = {
    //    def reverse(list: List[Int]) = (list :\ List.empty[Int]) ((element, foldingList) => foldingList ::: List(element))
    //
    //    println(reverse(List(1, 2, 3)))
    //    assert(List(3, 2, 1) == reverse(List(1, 2, 3)))
  }

  @Test
  def sortWith(): Unit = {
    assert(List("apple", "boy", "cart", "friday").sortWith(_.length < _.length) == List("boy", "cart", "apple", "friday"))
  }

  @Test
  def reduce(): Unit = {
    // ((1+2)*2 + 3)*2
    assert(18 == List(1, 2, 3).reduce((left, right) => (left + right) * 2))
    assert(18 == List(1, 2, 3).reduce(_ * 2 + _ * 2))

  }

  @Test
  def scan(): Unit = {
    // 0
    // 0+1
    // 0+1+2
    // 0+1+2+3
    // 0+1+2+3+4
    // 0+1+2+3+4+5
    assert(List(0, 1, 3, 6, 10, 15) == (1 to 5).scan(0)(_ + _))
    assert(List(0, 1, 3, 6, 10, 15) == (1 to 5).scanLeft(0)(_ + _))
    // 0+5+4+3+2+1
    // 0+5+4+3+2
    // 0+5+4+3
    // 0+5+4
    // 0+5
    // 0
    assert(Seq(15, 14, 12, 9, 5, 0) == (1 to 5).scanRight(0)(_ + _))
  }

  @Test
  def cons(): Unit = {
    var l = List(0)
    l ::= 1
    assert(1 :: List(0) == l)
  }

}
