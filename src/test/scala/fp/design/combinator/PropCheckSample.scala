package fp.design.combinator

import org.junit
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties, Test}

import scala.util.Random

object StringSpecification extends Properties("String") {
  property("startsWith") = forAll { (a: String, b: String) => (a + b).startsWith(a) }
  property("concat") = forAll { (a: String, b: String) => a.concat(b).startsWith(a) && a.concat(b).endsWith(b) }
}

object ListSpecification extends Properties("List") {
  val gen: Gen[(List[Int], Int)] = for {lst <- Gen.nonEmptyListOf(Gen.choose(0, 100))} yield (lst, lst(Random.nextInt(lst.length)))
  val lstgen: Gen[(List[Int], Int)] = for {
    lst <- Gen.nonEmptyListOf(Gen.choose(0, 100))
    e <- Gen.oneOf(lst)
  } yield (lst, e)
  val sameValLstGen: Gen[(List[Int], Int)] = for {
    lst <- Gen.nonEmptyListOf(Gen.const(5))
    e <- Gen.const(5)
  } yield (lst, e)
  property("max") = forAll(gen) { case (xs, x) => xs.contains(x) && xs.max >= x }
  property("contains") = forAll(lstgen) { case (xs, x) => xs.contains(x) }
  property("sum") = forAll(lstgen) { case (xs, _) => xs.sum == xs.reverse.sum } && forAll(sameValLstGen) { case (xs, x) => xs.sum == x * xs.size }
}

class PropCheckSample {
  @junit.Test
  def listPropSumChooseCheck(): Unit = {
    val intList = Gen.listOf(Gen.choose(0, 100))
    val sumProp = forAll(intList) { xs => xs.sum == xs.reverse.sum }
    assert(Test.check(sumProp)(identity).passed)
  }

  @junit.Test
  def listPropSumSameElementsCheck(): Unit = {
    val intList = Gen.listOf(Gen.const(Random.nextInt(100)))
    val sumProp = forAll(intList) { xs => xs.sum == xs.headOption.getOrElse(0) * xs.length }
    assert(Test.check(sumProp)(identity).passed)
  }

  @junit.Test
  def listPropMaxCheck(): Unit = {
    val gen = for {lst <- Gen.nonEmptyListOf(Gen.choose(0, 100))} yield (lst, lst.max)
    val maxProp = forAll(gen) { case (xs, mx) => xs.contains(mx) && xs.forall(mx >= _) }
    assert(Test.check(maxProp)(identity).passed)
  }

}
