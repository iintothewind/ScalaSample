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
  def arrayCompare(): Unit = {
    val left = 1.to(3).toArray
    val right = 3.to(1).toArray
    left.sameElements(right)
  }
}
