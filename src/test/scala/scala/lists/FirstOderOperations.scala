package scala.lists

import org.assertj.core.api.Assertions
import org.junit.Test

class FirstOderOperations {

  @Test(expected = classOf[NoSuchElementException])
  def basicOperations(): Unit = {
    val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
    val nums = 1 :: (2 :: (3 :: (4 :: Nil)))
    val diag3 = (1 :: (0 :: (0 :: Nil))) :: (0 :: (1 :: (0 :: Nil))) :: (0 :: (0 :: (1 :: Nil))) :: Nil
    val empty = Nil
    //java.util.NoSuchElementException: head of empty list
    assert(Nil.head)
    assert(empty.isEmpty)
    assert(fruit.nonEmpty)
    assert("apples" == fruit.head)
    assert("oranges" == fruit.tail.head)
    assert(List(1, 0, 0) == diag3.head)
  }

  @Test
  def listPatterns(): Unit = {
    val List(apples, oranges, pears) = List("apples", "oranges", "pears")
    Assertions.assertThat(apples.isInstanceOf[String]).isTrue
    Assertions.assertThat(oranges.isInstanceOf[String]).isTrue
    Assertions.assertThat(pears.isInstanceOf[String]).isTrue
  }

  @Test
  def consOperations(): Unit = {
    // cons operator :: can connect an element with a list
    assert(List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1)) == (1 :: (0 :: (0 :: Nil))) :: (0 :: (1 :: (0 :: Nil))) :: (0 :: (0 :: (1 :: Nil))) :: Nil)
    // list cons operator ::: can connect two lists to create a big list which contains all the elements of these two lists
    assert(List(1, 0, 0, 0, 1, 0, 0, 0, 1) == (1 :: (0 :: (0 :: Nil))) ::: (0 :: (1 :: (0 :: Nil))) ::: (0 :: (0 :: (1 :: Nil))))
  }

  @Test
  def concatenateLists(): Unit = {
    // list cons associates from left to right
    assert(List(1, 2, 3, 4, 5) == List(1, 2) ::: List(3, 4, 5))
    assert(List(1, 2, 3) == Nil ::: List(1, 2, 3))
  }

  @Test
  def concat(): Unit = {
    assert(List(1, 2, 3) == List.concat(List(1, 2), List(3)))
    assert(List(1, 2, 3) == List.concat(List(1, 2, 3), List()))
  }

  @Test
  def +:(): Unit = {
    assert(4 +: List(1, 2, 3) == List(4, 1, 2, 3))
  }

  @Test
  def :+(): Unit = {
    assert(List(1, 2, 3) :+ 4 == List(1, 2, 3, 4))
  }

  def connect[T](left: List[T], right: List[T]): List[T] = left match {
    case Nil => right
    case first :: rest => first :: connect(rest, right);
  }

  @Test
  def divideAndConquer(): Unit = {
    assert(List(1, 2, 3) == connect(Nil, List(1, 2, 3)))
    assert(List(1, 2, 3) == connect(List(), List(1, 2, 3)))
    assert(List(1, 2, 3) == connect(List(1), List(2, 3)))
    assert(List(1, 2, 3) == connect(List(1, 2), List(3)))
    assert(List(1, 2, 3) == connect(List(1, 2, 3), List()))
    assert(List(1, 2, 3) == connect(List(1, 2, 3), Nil))
  }

  @Test
  def testStringEmpty(): Unit = {
    def isEmpty1(s: String): Boolean = s == null || s.trim == ""
    // isEmpty2() is slower because it needs to traverse the whole list to find its end,
    // therefore, it takes more time in particular if the list is long
    def isEmpty2(s: String): Boolean = s == null || s.trim.length == 0
    assert(isEmpty1("     "))
    assert(isEmpty2("     "))
  }

  @Test
  def accessListEnd(): Unit = {
    val list = "abcde".toCharArray.toList

    val head = list.head
    val tail = list.tail
    assert('a' == head)
    assert("bcde".toCharArray.toList == tail)

    //unlike header and tail, which both run in constant time
    //init and last need to traverse the whole list to compute their result
    //therefor take time proportional to the length of the list
    val last = list.last
    val init = list.init
    assert('e' == last)
    assert("abcd".toCharArray.toList == init)
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case Nil => list
    case last :: rest => reverse(rest) ::: List(last)
  }


  @Test
  def testReverse(): Unit = {
    val list = "abcde".toCharArray.toList
    assert(list.reverse == reverse(list))
  }

  @Test
  def reverseLaws(): Unit = {
    val list = "abcde".toCharArray.toList
    assert(list == list.reverse.reverse)
    assert(list.reverse.init == list.tail.reverse)
    assert(list.reverse.tail == list.init.reverse)
    assert(list.reverse.head == list.last)
    assert(list.reverse.last == list.head)
  }

  @Test
  def take(): Unit = {
    val list = "abcde".toCharArray.toList
    assert(Nil == list.take(-1))
    assert(Nil == list.take(0))
    assert(List('a', 'b') == list.take(2))
    assert(list == list.take(9))
  }

  @Test
  def drop(): Unit = {
    val list = "abcde".toCharArray.toList
    assert(list == list.drop(-1))
    assert(list == list.drop(0))
    assert(List('c', 'd', 'e') == list.drop(2))
    assert(Nil == list.drop(9))
  }

  @Test
  def splitAt(): Unit = {
    val list = "abcde".toCharArray.toList
    def assertSplitAt(position: Int): Unit = {
      val (taken, dropped) = list.splitAt(position)
      assert(taken == list.take(position))
      assert(dropped == list.drop(position))
    }

    assertSplitAt(-1)
    assertSplitAt(0)
    assertSplitAt(2)
    assertSplitAt(9)
  }

  @Test
  def elementSelection(): Unit = {
    val list = "abcde".toCharArray.toList
    assert('c' == list.apply(2))
    assert('c' == list(2))
    assert(list(2) == list.drop(2).head)
    assert(Range(0, 5) == list.indices)
  }

  @Test
  def flatten(): Unit = {
    assert(List(1, 2, 3, 4, 5) == List(List(1, 2), List(3), List(), List(4, 5)).flatten)
    assert(List('a', 'p', 'p', 'l', 'e', 's', 'o', 'r', 'a', 'n', 'g', 'e', 's', 'p', 'e', 'a', 'r', 's')
      == List("apples", "oranges", "pears").map(_.toCharArray).flatten)
  }

  @Test
  def zip(): Unit = {
    val nums = List(1, 2, 3)
    val list = "abcde".toCharArray.toList
    assert(List((1, 'a'), (2, 'b'), (3, 'c')) == nums.zip(list))
    assert(List(('a', 1), ('b', 2), ('c', 3)) == list.zip(nums))
    assert(List((0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e')) == list.indices.zip(list))

  }

  @Test
  def unzip(): Unit = {
    val zipped = List((1, 'a'), (2, 'b'), (3, 'c'))
    val (nums, chars) = zipped.unzip
    assert(List(1, 2, 3) == nums)
    assert(List('a', 'b', 'c') == chars)
  }

  @Test
  def display(): Unit = {
    val nums = List(1, 2, 3)
    val list = "abcde".toCharArray.toList
    assert("List(1, 2, 3)" == nums.toString())
    assert("[a,b,c,d,e]" == list.mkString("[", ",", "]"))
  }

  @Test
  def testToArray(): Unit = {
    val nums = List(1, 2, 3)
    val array = Array(1, 2, 3)
    assert((array.toTraversable, nums.toArray.toIterable).zipped.forall(_ == _))
    assert(array.zip(nums.toArray[Int]).forall(pair => pair._1 == pair._2))
    assert(nums == nums.toArray.toList)
  }

  @Test(expected = classOf[ArrayIndexOutOfBoundsException])
  def testCopyToArray(): Unit = {
    val array = Range(1, 10).toArray[Int]
    val nums = List(4, 5, 6)
    val list = List(1, 2, 3)
    nums.copyToArray(array)
    assert(nums.toArray[Int].zip(array.take(3)).forall(pair => pair._1 == pair._2))
    //java.lang.ArrayIndexOutOfBoundsException: -1
    list.copyToArray(array, -1)
    assert(Array(1, 2, 3).zip(array.take(3)).forall(pair => pair._1 == pair._2))
    list.copyToArray(array, 0)
    assert(Array(1, 2, 3).zip(array.take(3)).forall(pair => pair._1 == pair._2))
    list.copyToArray(array, 99)
    assert(List(4, 5, 6).zip(array.take(3)).forall(pair => pair._1 == pair._2))
  }


  @Test
  def testMergeSort(): Unit = {
    def mergeSort[T](list: List[T])(less: (T, T) => Boolean): List[T] = {
      def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
        case (Nil, _) => right
        case (_, Nil) => left
        case (leftFirst :: leftRest, rightFirst :: rightRest) => if (less(leftFirst, rightFirst)) leftFirst :: merge(leftRest, right) else rightFirst :: merge(left, rightRest)
      }
      val middle = list.length / 2
      if (middle == 0) list
      else {
        val (left, right) = list.splitAt(middle)
        merge(mergeSort(left)(less), mergeSort(right)(less))
      }
    }
    val list = List(9, 1, 5, 4, 3, 7, 8, 5, 2, 7, 6)
    assert(List(1, 2, 3, 4, 5, 5, 6, 7, 7, 8, 9) == mergeSort(list)(_ < _))
  }


}
