package war

import org.junit.Test

import scala.collection.mutable.ListBuffer

class Solution {
  def removeDuplicates(nums: Array[Int]): Int = {
    nums.toSet.size
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

  @Test
  def testMaxProfit(): Unit = {
    assert(maxProfit(Array(1, 2)) == 1)
    assert(maxProfit(Array(1, 4, 2)) == 3)
    assert(maxProfit(Array(3, 2, 6, 5, 0, 3)) == 7)
    assert(maxProfit(Array(100, 180, 260, 310, 40, 535, 695)) == 865)
  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
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
    foldl(buffer.sorted.reverse: _*)
  }

}
