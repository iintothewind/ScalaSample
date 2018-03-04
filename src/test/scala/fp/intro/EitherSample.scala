package fp.intro

import org.junit.Test

class EitherSample {
  @Test
  def testFromOption(): Unit = {
    assert(Ether.fromOption(None).isLeft)
    assert(Rht("admin") == Ether.fromOption(Some("admin")))
  }

  @Test
  def testToOption(): Unit = {
    assert(Lft(new IllegalArgumentException("a")).toOption.isEmpty)
    assert(Rht("admin").toOption.contains("admin"))
  }

  @Test
  def testFlatMap(): Unit = {
    assert(Lft(new IllegalArgumentException("a")).flatMap(Rht(_)).isLeft)
    assert(Rht("12") == Rht(12).flatMap(x => Rht(x.toString)))
  }

  @Test
  def testMap(): Unit = {
    assert(Lft[Exception, String](new IllegalArgumentException("a")).map(_.toInt).isLeft)
    assert(Rht[Exception, String]("a").map(_.toInt).isLeft)
    assert(Rht(12) == Rht("12").map(_.toInt))
  }

  @Test
  def testMap2(): Unit = {
    assert(Lft[Exception, Int](new IllegalArgumentException("a")).map2(Rht(2))(_ + _).isLeft)
    assert(Rht(1).map2(Lft[Exception, Int](new IllegalArgumentException("b")))(_ + _).isLeft)
    assert(Rht(12) == Rht("1").map2(Rht("2"))((a, b) => a.concat(b).toInt))
  }

}