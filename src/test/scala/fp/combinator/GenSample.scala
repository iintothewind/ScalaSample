package fp.combinator

import org.junit.Test

class GenSample {
  @Test
  def testUnit(): Unit = {
    Gn.const(0).sample.ensuring(_ == 0)
  }

  @Test
  def testRandInt(): Unit = {
    println(Gn.randInt.sample)
  }

  @Test
  def testBetween(): Unit = {
    Gn.between(1, 10).sample.ensuring(n => n >= 1 && n < 10)
  }

  @Test
  def testMany(): Unit = {
    Gn.between(1, 10).many(Gn.between(1, 10)).sample.ensuring(it => it.forall(n => (1 to 10).contains(n)))
  }

  @Test
  def testOneOf(): Unit = {
    "abcde".ensuring(it => it.contains(Gn.oneOf(Gn.const(it: IndexedSeq[Char])).sample))
  }

  @Test
  def testRandString(): Unit = {
    println(Gn.randLetters(Gn.between(5, 9))(Gn.between(5, 10)).sample)
  }

  @Test
  def testString(): Unit = {
    println(IndexedSeq("abc").foreach(s => println(s.getClass)))
  }

}
