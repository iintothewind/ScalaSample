package scala.convert

import org.junit.Test

import scala.language.implicitConversions

class ApplicationScenarios {

  @Test
  def convertToAnExpectedType(): Unit = {
    implicit def doubleToInt(x: Double): Int = x.toInt
    val i: Int = 3.5
    assert(3 == i)
  }

  @Test
  def convertTheReceiver_InteractWithNewTypes(): Unit = {
    class Baby(val smile: Int) {}
    implicit def bbToInt(baby: Baby): Int = baby.smile
    val smiles = 0
    // allow Int.+() to accept Baby type
    assert(9 == smiles + new Baby(9))
  }

  @Test
  def convertTheReceiver_SimulateNewSyntax(): Unit = {
    // <-> operator is implicitly defined in class MagicWand
    assert("1221" == (1 <-> 2))
    assert("abcdcdab" == ("ab" <-> "cd"))
  }

  /**
   * parameter name depends the default value defined in package object
   */
  def greet(greeting: String)(implicit name: String): String = {
    greeting + ", " + name
  }

  @Test
  def implicitParameters(): Unit = {
    assert("Hi, there" == greet("Hi"))
    assert("Hi, Sarah" == greet("Hi")("Sarah"))
  }

  /*
   *T has to be a subtype of Ordered
   */
  def orderedMax[T <: Ordered[T]](list: List[T]): T = list match {
    case Nil => null.asInstanceOf[T]
    case List(x) => x
    case first :: rest =>
      val maxRest = orderedMax(rest)
      if (first > maxRest) first else maxRest
  }

  /**
   * implicitMax() does not require T to be a subtype of Ordered, moreover,
   * by specifying ordering, you get the max in your own way
   */
  def implicitMax[T](list: List[T])(implicit ordering: Ordering[T]): T = list match {
    case Nil => null.asInstanceOf[T]
    case List(x) => x
    case first :: rest =>
      val maxRest = implicitMax(rest)(ordering)
      if (ordering.gteq(first, maxRest)) first else maxRest
  }

  /**
   * poorOrdering depends on the default value defined in package object
   */
  def poorStyleMax[T](list: List[T])(implicit poorOrdering: (T, T) => Boolean): T = list match {
    case Nil => null.asInstanceOf[T]
    case List(x) => x
    case first :: rest =>
      val maxRest = poorStyleMax(rest)(poorOrdering)
      if (poorOrdering.apply(first, maxRest)) first else maxRest
  }

  /**
   * <% is an deprecated operator
   * T <% Ordered[T] means I can use any T, so long as T can be treated as an Ordered[T]
   */
  def viewBoundMax[T <% Ordered[T]](list: List[T]): T = list match {
    case Nil => null.asInstanceOf[T]
    case List(x) => x
    case first :: rest =>
      val maxRest = viewBoundMax(rest)
      if (first > maxRest) first else maxRest
  }


  @Test
  def testMax(): Unit = {
    val list = List(IdPig(1), IdPig(2), IdPig(3))
    assert(3 == orderedMax(list).id)
    assert(3 == viewBoundMax(list).id)
    assert(3 == implicitMax(list).id)
    assert(1 == implicitMax(list)(Ordering[IdPig].reverse).id)
    assert(3 == poorStyleMax(list).id)
  }

}
