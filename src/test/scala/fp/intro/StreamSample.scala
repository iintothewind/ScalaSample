package fp.intro

import org.junit.Test

class StreamSample {
  @Test
  def testCons(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
  }

  @Test
  def testAppend(): Unit = {
    val pre = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    val st = {
      print(4)
      4
    } #:: {
      print(5)
      5
    } #:: {
      print(6)
      6
    } #:: Strim.empty[Int]
    // 5,6 will not be evaluated
    val t = pre #::: st
  }

  @Test
  def testLength(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    assert(st.length == 3)
  }


  @Test
  def testHeadOptionByStrictness(): Unit = {
    val x = Cns(() => {
      print("a")
      "a"
    }, () => Nis)
    val h1 = x.headOption
    val h2 = x.headOption
  }

  @Test
  def testHeadOptionByLaziness(): Unit = {
    val x = {
      print("a")
      "a"
    } #:: "b" #:: Strim.empty[String]
    val h1 = x.headOption
    val h2 = x.headOption
  }

  @Test
  def testHeadOption(): Unit = {
    assert(Nis.headOption.isEmpty)
    assert(1 == Strim(1, 2).headOption.get)
  }

  @Test
  def testReverse(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    st.reverse
  }

  @Test
  def testToList(): Unit = {
    assert(List(1, 2) == (1 #:: 2 #:: Strim.empty).toList)
  }

  @Test
  def testTake(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: {
      print(4)
      4
    } #:: {
      print(5)
      5
    } #:: Strim.empty[Int]
    st.take(2)
  }

  @Test
  def testFind(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: {
      print(4)
      4
    } #:: {
      print(5)
      5
    } #:: Strim.empty[Int]
    assert(st.find(_ == 3).isDefined)
  }

  @Test
  def testExists(): Unit = {
    val st = {
      print("a")
      "a"
    } #:: {
      print("b")
      "b"
    } #:: {
      print("c")
      "c"
    } #:: Strim.empty[String]
    assert(st.exists(_ == "a"))
  }

  @Test
  def testForall(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    assert(!st.forall(_ < 2))
  }

  @Test
  def testTakeWhile(): Unit = {
    val st = {
      print(1)
      1
    } #:: {
      print(2)
      2
    } #:: {
      print(3)
      3
    } #:: Strim.empty[Int]
    assert(Nis == st.takeWhile(_ < -2))
  }

  @Test
  def testFlatMap(): Unit = {
    assert(List(1, 1, 2, 2) == Strim(1, 2).flatMap((x) => x #:: x #:: Nis).toList)
  }

  @Test
  def testFilter(): Unit = {
    assert(List(1) == Strim(1, 2).filter(_ < 2).toList)
  }

  @Test
  def testConstant(): Unit = {
    assert(List(1, 1, 1) == Strim.constant(1).take(3).toList)
    assert(Strim.constant(1).take(3).toList == Strim.unfold(1)(_ => Some((1, 1))).take(3).toList)
  }

  @Test
  def testFrom(): Unit = {
    assert(List(0, 1, 2) == Strim.from(0, 1).take(3).toList)
    assert(Strim.from(0, 1).take(3).toList == Strim.unfold(0)(s => Some((s, s + 1))).take(3).toList)
  }

  @Test
  def testFibonacci(): Unit = {
    assert(List(5, 3, 2, 1, 1, 0) == Strim.fibonacci(5).toList)
    assert(Strim.unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2)))).take(6).toList == Strim.fibonacci(5).reverse.toList)
  }

  @Test
  def testZip(): Unit = {
    assert(List((1, 4), (2, 5), (3, 6)) == Strim(1, 2, 3).zip(Strim(4, 5, 6)).toList)
  }

  @Test
  def testStartsWith(): Unit = {
    assert(Strim(1, 2, 3).startsWith(Strim(1, 2)))
  }
}