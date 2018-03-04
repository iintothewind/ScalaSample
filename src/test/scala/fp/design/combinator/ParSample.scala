package fp.design.combinator

import org.junit.Test

import scala.util.Try

class ParSample {
  @Test
  def testAsync(): Unit = {
    val ps = List("a", "b", "c").map(Par.async(i => Try(i.toInt).recover { case _ => 0 }))
    ps.map(_.run()).foreach(_.ensuring(_.isSuccess))
  }

  @Test
  def testAsyncWithoutRecovery(): Unit = {
    Par.lazyUnit(Try("a".toInt)).run().isFailure
  }

  @Test
  def testMap2(): Unit = {
    val mp2 = Par.map2(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }), Par.lazyUnit(Try("b".toInt).recover { case _ => 0 }))((l, r) => Try(l + r))
    mp2.run().foreach(_.ensuring(_ == 0))
  }

  @Test
  def testMap(): Unit = {
    Par.map(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }))(i => Try(i + 1)).run().foreach(_.ensuring(_ == 1))
  }

  @Test
  def testSortPar(): Unit = {
    val lst = List(9, -2, 3, -6, 5, 2, 1)
    Par.sortPar(Par.lazyUnit(Try(lst))).run().foreach(_.ensuring(_ == lst.sorted))
  }

  @Test
  def testParMap(): Unit = {
    Par.parMap(List.range(1, 100))(i => Try(i * 2)).run().foreach(_.ensuring(_ == List.range(1, 100).map(_ * 2)))
  }

  @Test
  def testParMapWithoutRecovery(): Unit = {
    Par.parMap(List("b", "a", "2", "3", "4", "5"))(i => Try(i.toInt)).run().isFailure
  }

  @Test
  def testChooser(): Unit = {
    Par.unit(Try(List(9, -2, 3, -6, 5, 2, 1))).chooser(xs => Par.lazyUnit(Try(xs.max))).run().foreach(_.ensuring(_ == 9))
  }
}