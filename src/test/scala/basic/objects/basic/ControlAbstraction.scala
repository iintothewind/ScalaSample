package basic.objects.basic

import java.io.PrintStream
import java.net.URI
import java.time.LocalDate

import org.junit.Test

import scala.collection.Iterator
import scala.io.Source

class ControlAbstraction {
  @Test
  def placeholderSyntax(): Unit = {
    val list = List(-11, -10, -5, 0, 1, 2, 3)
    println(list.filter(_ > 0).mkString(","))
  }

  @Test
  def placeholderForParameters(): Unit = {
    val add = (_: Int) + (_: Int)
    println(add(1, 2))
    val plusOne = (_: Int) + 1
    println(plusOne(2))
  }

  @Test
  def partiallyAppliedFunction(): Unit = {
    def sum(a: Int, b: Int, c: Int) = a + b + c

    assert(6 == sum(1, 2, 3))
    val a = sum _
    assert(6 == a(1, 2, 3))
    val b = sum(1, _: Int, 3)
    assert(6 == b(2))
  }

  @Test
  def closureSample(): Unit = {
    val more = 1
    val addMore = (x: Int) => x + more
    assert(7 == addMore(6))
  }

  @Test
  def repeatedParameters(): Unit = {
    def echo(args: String*): Unit = args.foreach(println)

    echo("hello", "world")
    val array = Array("what's", "up", "doc?")
    // append the array argument with a colon and an _* symbol
    // this notation tells the compiler to pass each element of arr as its own argument to eacho
    // rather than all of it as a single argument
    echo(array.toSeq: _*)
  }

  @Test
  def namedArguments(): Unit = {
    def speed(distance: Float, time: Float): Float = distance / time

    assert(10f == speed(100f, 10f))
    assert(10f == speed(distance = 100f, time = 10f))
    assert(10f == speed(time = 10f, distance = 100f))
  }

  @Test
  def defaultParameterValues(): Unit = {
    def printDate(out: PrintStream = Console.out): Unit = {
      out.println("today = " + LocalDate.now().toString)
    }

    printDate()
    printDate(Console.err)
  }

  @Test
  def simplifyClientCode(): Unit = {
    val list = List(1, 2, 3, 4)
    assert(list.exists(_ % 2 == 0))
    assert(list.exists(_ >= 3))
  }

  @Test
  def curry(): Unit = {
    def curriedSum(x: Int)(y: Int): Int = x + y

    assert(3 == curriedSum(1)(2))
    val onePlus = curriedSum(1) _
    assert(3 == onePlus(2))
    val twoPlus = curriedSum(2) _
    assert(4 == twoPlus(2))

    def first(x: Int) = (y: Int) => x + y

    val second = first(1)
    assert(3 == second(2))

    def sort[T](list: List[T])(compare: (T, T) => Boolean): List[T] = list.sortWith(compare)

    assert(List(1, 2, 3, 4) == sort(List(4, 3, 2, 1))(_ < _))

    def withFile(uri: URI)(filter: Iterator[String] => List[String]): List[String] = filter(Source.fromFile(uri).getLines())

    withFile(getClass.getResource("/country.csv").toURI)(iterator => iterator.toList.filter(_.contains("an"))).foreach(println)
  }

  @Test(expected = classOf[ArithmeticException])
  def byNameParameters(): Unit = {
    var assertionEnabled = true

    def myAssert(predicate: () => Boolean): Unit = if (assertionEnabled && !predicate()) throw new AssertionError

    myAssert(() => 5 > 3)
    //myAssert(5>3) // wont work, because of missing ()=>

    assertionEnabled = false

    // predicate is not by name parameter,
    // because is a Boolean, the expression inside the parentheses in boolAssert(5/0) is evaluated
    // before
    // the call to boolAssert, thus pops up the java.lang.ArithmeticException: / by zero
    def boolAssert(predicate: Boolean): Unit = if (assertionEnabled && !predicate) throw new AssertionError

    boolAssert(5 / 0 == 0) // assertionEnabled = false, but exception still pops up

    assertionEnabled = true

    def byNameAssert(predicate: => Boolean): Unit = if (assertionEnabled && !predicate) throw new AssertionError

    byNameAssert(5 > 3)
  }

  @Test(expected = classOf[RuntimeException])
  def tailRecursive(): Unit = {
    def boom(x: Int): Int = if (x == 0) throw new RuntimeException("boom!") else boom(x - 1) + 1
    // should be enabled by scalac
    boom(3)
  }

}
