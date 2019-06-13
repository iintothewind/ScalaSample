package war

import org.junit.Test

import scala.collection.mutable.ListBuffer

class Solution {
  def removeDuplicates(nums: Array[Int]): Int = {
    nums.toSet.size
  }

  @Test
  def testMaxProfit(): Unit = {
    assert(maxProfit(Array(1, 2)) == 1)
    assert(maxProfit(Array(1, 4, 2)) == 3)
    assert(maxProfit(Array(3, 2, 6, 5, 0, 3)) == 7)
    assert(maxProfit(Array(100, 180, 260, 310, 40, 535, 695)) == 865)
  }

  def maxProfit(prices: Array[Int]): Int = {
    def loop(profit: Int, lowest: Int, highest: Int, xs: List[Int]): Int = {
      xs match {
        case x :: rs if x >= highest => loop(profit, lowest, x, rs)
        case x :: rs if x < highest => loop(profit + (highest - lowest), x, x, rs)
        case _ => profit + (highest - lowest)
      }
    }

    Option(prices).map(_.toList) match {
      case Some(xs) if xs.size > 1 => loop(0, xs.head, xs.head, xs.tail)
      case _ => 0
    }
  }

  def foldl(lst: Int*): ListNode = lst.foldLeft(null: ListNode)((node, i) => {
    val nd = new ListNode(i)
    nd.next = node
    nd
  })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): List[A] = f(z) match {
    case None => List.empty[A]
    case Some((a, s)) => a :: unfold(s)(f)
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val s1: List[Long] = unfold[Long, ListNode](l1)(n => Option(n).map(nd => (nd.x, nd.next)))
    val s2: List[Long] = unfold[Long, ListNode](l2)(n => Option(n).map(nd => (nd.x, nd.next)))
    foldl((BigDecimal(s1.reverse.mkString) + BigDecimal(s2.reverse.mkString)).toString.map(_.toString.toInt): _*)
  }

  @Test
  def testAddTwoNumbers(): Unit = {
    println(unfold(addTwoNumbers(foldl(2, 4, 3), foldl(5, 6, 4)))(n => Option(n).map(nd => (nd.x, nd.next))))
    println(unfold(addTwoNumbers(foldl(9), foldl(1, 9, 9, 9, 9, 9, 9, 9, 9, 9)))(n => Option(n).map(nd => (nd.x, nd.next))))
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    val buffer = ListBuffer.empty[Int]
    for (node <- lists) {
      buffer ++= unfold(node)(n => Option(n).map(nd => (nd.x, nd.next)))
    }
    foldl(buffer.sorted.reverse.toSeq: _*)
  }

  @Test
  def testReverse(): Unit = {
    println(reverse(120))
    println(reverse(123))
    println(reverse(0))
    println(reverse(-123))
    println(reverse(1534236469))
    println(reverse(-2147483648))
  }

  def reverse(x: Int): Int = x.toLong match {
    case b if b == 0L || b > Int.MaxValue => 0
    case a if a > 0 => a.toString.reverse.dropWhile(_ == '0').toLong match {
      case m if m > Int.MaxValue => 0
      case n => n.toInt
    }
    case c => ('-' +: c.toString.reverse.filterNot(_ == '-').dropWhile(_ == '0') match {
      case m if m.toLong < Int.MinValue => "0"
      case n => n
    }).toInt
  }

  //  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
  //    def median(arr1: Array[Int], arr2: Array[Int]): Double = Array.concat(arr1, arr2).splitAt((arr1.length + arr2.length) / 2) match {
  //      case (l, r) if l.length < r.length => r.head.toDouble
  //      case (l, r) if l.length == r.length => (l.last.toDouble + r.head.toDouble) / 2D
  //      case _ => 0D
  //    }
  //
  //    def loop(arr1: Array[Int], arr2: Array[Int]): Double = (arr1, arr2) match {
  //      case (Array(), Array()) => 0D
  //      case (Array(i), Array()) => i.toDouble
  //      case (Array(i), b@Array(_)) => val (l, r) = b.span(n => n <= i); median(l :+ i, r)
  //      case (Array(), Array(i)) => i.toDouble
  //      case (a@Array(_), Array(i)) => val (l, r) = a.span(n => n <= i); median(l :+ i, r)
  //      case (Array(a), Array(b)) => (a.toDouble + b.toDouble) / 2
  //      case (a, b) if a.last <= b.head => median(a, b)
  //      case (a, b) if a.head >= b.last => median(b, a)
  //      case (a, b) if a.length > 1 && b.length > 1 && a.head <= b.head => loop(a.splitAt(1)._2, b.dropWhile(n => n <= a.head))
  //      case (a, b) if a.length > 1 && b.length > 1 && a.last >= b.last => loop(a.splitAt(1)._2, b.splitAt(1)._2)
  //
  //    }
  //
  //
  //  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

}
