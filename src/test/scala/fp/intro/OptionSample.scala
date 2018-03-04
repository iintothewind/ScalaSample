package fp.intro

import org.junit.Test

class OptionSample {
  def variance(xs: Seq[Double]): Opt[Double] = Opt(xs)
    .filter(_.nonEmpty)
    .map(xs => xs.map(x => math.pow(x - (xs.sum / xs.length), 2.0D)).sum / xs.length)

  def standardDeviation(xs: Seq[Double]): Opt[Double] = variance(xs).map(math.sqrt)

  @Test
  def testVariance(): Unit = {
    assert(Non == variance(null))
    assert(Non == variance(Seq.empty[Double]))
    assert(4.0D == variance(Seq(-3, -2, -1, 0, 1, 2, 3)).getOrElse(0d))
    assert(standardDeviation(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)).nonEmpty)
  }

  // Opt.lift() can `lift` ordinary functions to functions that operate on Opt
  def optAbs: Opt[Int] => Opt[Int] = Opt.lift(math.abs)

  @Test
  def testLift(): Unit = {
    assert(Non == optAbs(Non))
    assert(Non == optAbs(Opt.empty[Int]))
    assert(9 == optAbs(Opt(-9)).get)
  }

  @Test
  def testMap(): Unit = {
    assert(Opt("1").map(_.toInt).isDefined)
    assert(Opt("").map(_.toInt).isEmpty)
  }

  @Test
  def testMap2(): Unit = {
    assert(Opt("1s").map2(Opt("0"))((l, r) => l.concat(r).toInt).isEmpty)
    assert(10 == Opt("1").map2(Opt("0"))((l, r) => l.concat(r).toInt).get)
  }

  @Test
  def testForComprehension(): Unit = {
    Opt("One").flatMap(s => Opt(s.length)).flatMap(i => Opt(i % 2 == 0)).get.ensuring(_ == false)
    for {s <- Opt.empty[String]
         i <- Opt(s.length)
         even <- Opt(i % 2 == 0)
    } even.ensuring(_ == true) // does not matter, ensuring will never be executed
  }

  @Test
  def testSequence(): Unit = {
    assert(Opt.sequence(Nil).isEmpty)
    assert(Opt.sequence(List(Sme(1), Non, Sme(3))).isEmpty)
    assert(Sme(1 :: 2 :: 3 :: Nil) == Opt.sequence(List(Sme(1), Sme(2), Sme(3))))
    assert(Opt.sequence(List("1", "a", "3").map(x => Opt.attempt(x.toInt))).isEmpty)
    assert(Sme(1 :: 2 :: 3 :: Nil) == Opt.sequence(List("1", "2", "3").map(x => Opt.attempt(x.toInt))))
  }

  @Test
  def testTraverse(): Unit = {
    assert(Non == Opt.traverse(List("1", "a", "3"))(_.toInt))
    assert(Sme(1 :: 2 :: 3 :: Nil) == Opt.traverse(List("1", "2", "3"))(_.toInt))
  }
}