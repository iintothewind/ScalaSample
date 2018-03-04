package fp.design.combinator

import org.junit.Test

class PrpSample {
  @Test
  def testForAll(): Unit = {
    Prp.forAll(Gn.between(1, 10))(i => i >= 1 && i < 10).check
  }

  @Test
  def testAnd(): Unit = {
    val p1: Prp = Prp.forAll(Gn.between(1, 10))(i => i >= 1 && i < 10)
    val p2: Prp = Prp.forAll(Gn.oneOf(Gn.const(IndexedSeq(1, 2, 3, 4, 5))))(i => i >= 1 && i <= 5)
    (p1 && p2).check
  }

  @Test
  def testOr(): Unit = {
    val p1: Prp = Prp.forAll(Gn.between(1, 10))(i => i > 10)
    val p2: Prp = Prp.forAll(Gn.randLetters(Gn.const(5))(Gn.const(5)))(seq => seq.forall(_.length == 5))
    (p1 || p2).check
  }


  @Test
  def testThrows(): Unit = {
    Prp.forAll(Gn.between(-3, 3))(i => i / i == 1).check
    Prp.throws(classOf[ArithmeticException], Gn.const(0))(i => i / i == 1).check
  }
}
