package basic.collects.lists

import org.junit.Test


class Literals {
  @Test
  def testListLiterals(): Unit = {
    val fruit = List("apples", "oranges", "pears")
    val nums = List(1, 2, 3, 4)
    val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    val empty = List()
    fruit.foreach(println)
    nums.foreach(println)
    diag3.foreach(println)
    empty.foreach(println)
  }

  @Test
  def testListType(): Unit = {
    //This means that for each pair of types S and T, if S is a subtype of T, then List[S] is a subtype of List[T]
    // Nothing is a subtype of every other scala type
    val empty: List[Nothing] = List()
    val xs: List[String] = List()
    println(empty)
    println(xs)
    println(Nil)
  }

  @Test
  def constructingLists(): Unit = {
    val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
    val nums = 1 :: (2 :: (3 :: (4 :: Nil)))
    val diag3 = (1 :: (0 :: (0 :: Nil))) :: (0 :: (1 :: (0 :: Nil))) :: (0 :: (0 :: (1 :: Nil))) :: Nil
    val empty = Nil
  }

  @Test
  def apply(): Unit = {
    List.apply(1, 2, 3)
  }

  @Test
  def range(): Unit = {
    assert(List(1, 3, 5, 7) == List.range(1, 9, 2))
  }

  @Test
  def fill(): Unit = {
    assert("aaaaa".toCharArray.toList == List.fill(5)('a'))
    assert(List(List('a', 'a', 'a'), List('a', 'a', 'a')) == List.fill(2, 3)('a'))
  }

  @Test
  def tabulate(): Unit = {
    assert(List.range(0, 5, 1) == List.tabulate(5)(n => n))
    assert(List(0, 1, 4, 9, 16) == List.tabulate(5)(n => n * n))
    assert(List(List(0, 1, 2, 3, 4), List(0, 1, 2, 3, 4), List(0, 1, 2, 3, 4), List(0, 1, 2, 3, 4), List(0, 1, 2, 3, 4)) == List.tabulate(5, 5)((m, n) => n))
  }

  @Test
  def zipped(): Unit = {
    assert(List(20, 60) == (List(10, 20), List(2, 3, 4)).zipped.map(_ * _))
    assert((List("abc", "de"), List(3, 2)).zipped.forall(_.length == _))
  }

}
