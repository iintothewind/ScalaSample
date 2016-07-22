package fp.intro

import org.junit.Test

import scala.annotation.tailrec


object Hof {
  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)
    go(n, 1)
  }


  def fibonacci(n: Int): Int = {
    @tailrec
    def fibTail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fibTail(n - 1, b, a + b)
    }
    fibTail(n, 0, 1)
  }

  // use curry to improve the type inference
  def findFirst[A](as: Seq[A])(p: A => Boolean): Int = {
    @tailrec
    def find(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else find(n + 1)
    find(0)
  }

  // use curry to improve the type inference
  def isSorted[A](as: Seq[A])(ordered: (A, A) => Boolean): Boolean = {
    as.zip(as.sortWith(ordered)).forall(pair => pair._1 == pair._2)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult[A](input: A, f: A => A): String = {
    s"The result of f($input) is ${f(input)}"
  }
}


class HofSample {
  @Test
  def testFibonacci(): Unit = {
    assert(0 == Hof.fibonacci(0))
    assert(1 == Hof.fibonacci(1))
    assert(8 == Hof.fibonacci(6))
  }

  @Test
  def testFormatResult(): Unit = {
    println(Hof.formatResult(3, Hof.fibonacci))
  }

  @Test
  def testFindFirst(): Unit = {
    assert(0 == Hof.findFirst(Array("apple", "pear", "orange"))(_.contains("pp")))
  }

  @Test
  def testIsSorted(): Unit = {
    assert(Hof.isSorted(Seq(1, 2, 3, 4, 5))(_ < _))
  }
}
