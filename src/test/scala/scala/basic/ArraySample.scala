package scala.basic

import org.junit.Test

class ArraySample {
  @Test
  def stringArray(): Unit = {
    val greetings = new Array[String](4)
    greetings(0) = "Hello"
    greetings.update(1, ",")
    greetings(2) = "Scala"
    greetings(3) = "\n"
    for (greeting <- greetings) {
      print(greeting)
    }
  }

  @Test
  def apply(): Unit = {
    val greetings = Array.apply("Hello", ",", "Scala", "\n")
    greetings.foreach(print)
  }

  @Test
  def typeInferArray(): Unit = {
    val greetings = Array("Hello", ",", "Scala", "\n")
    greetings.foreach(print)
  }

  @Test
  def testCompare(): Unit = {
    val left = 1.to(3).toArray
    val right = 1.to(3).toArray
    val reversed = 3.to(1).toArray
    left.sameElements(right).ensuring(_ == true) // same elements with same order
    left.sameElements(reversed).ensuring(_ == false)
  }

  @Test
  def testDeepCompare(): Unit = {
    val left = 1.to(3).toArray
    val right = 1.to(3).toArray
    assert(left.deep == right.deep)
  }

  @Test
  def testCorresponds(): Unit = {
    val left = 1.to(3).toArray
    val right = 1.to(3).toArray
    left.corresponds(right)(_ == _)
  }
}
