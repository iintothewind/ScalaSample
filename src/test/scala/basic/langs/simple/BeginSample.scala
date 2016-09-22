package basic.langs.simple

import org.junit.Test

class BeginSample {
  @Test
  def defineValSample(): Unit = {
    val msg = "hello, world!"
    val msg2: String = "Hello again, world!"
    println(msg, msg2)
    var msg3 = "Hello, world"
    msg3 = "Hello, Scala"
    println(msg3)
  }

  @Test
  def defineFunction(): Unit = {
    def max(x: Int, y: Int): Int = {
      if (x > y) x
      else y
    }
    println(max(3, 5))
  }

  @Test
  def arraySample(): Unit = {
    val greetings = new Array[String](3)
    greetings(0) = "Hello"
    greetings(1) = ","
    greetings(2) = "world!\n"
    for (i <- 0 to 2)
      print(greetings(i))
    greetings.update(2, "scala\n")
    greetings.foreach(print)
    val things = Array(1, "2", '3')
    things.foreach(item => println(item.getClass))
  }

  @Test
  def zip(): Unit = {
    assert((Array(1, 2, 3) zip Array("a", "b", "c")).mkString(" ") == Array((1, "a"), (2, "b"), (3, "c")).mkString(" "))
    assert((Array(1, 2, 3) zip Array("a", "b")).mkString(" ") == Array((1, "a"), (2, "b")).mkString(" "))
  }

  @Test
  def equals(): Unit = {
    assert(421 == 421)
    assert(new String("abc") == new String("abc"))

    /*
    assert(!(new String("abc") eq new String("abc")))
    assert(new String("abc") ne new String("abc"))
     */
  }

  @Test
  def nothing(): Unit = {
    def error(message: String): Nothing = throw new RuntimeException(message)
    def divide(x: Int, y: Int): Int = if (y != 0) x / y else error("cannot divide by zero")
  }
}
