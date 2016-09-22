package basic.langs.simple

import org.junit.{Assert, Test}

class Foundation {
  @Test
  def valSample(): Unit = {
    val msg1 = "Hello, world!"
    val msg2: String = "Hell again, world!"
    println(msg1, msg2)
  }

  @Test
  def varSample(): Unit = {
    var msg = "Hello, world!"
    msg = "Hello,Scala!"
    println(msg)
    var i = (1).+(2)
    Assert.assertEquals(1 + 2, i)
  }

  @Test
  def functionSample(): Unit = {
    def max(left: Int, right: Int): Int = {
      if (left > right) left
      else right
    }
    println(max(3, 5))
  }

  @Test
  def loopSample(): Unit = {
    val list = List("one", "two", "three", "four")
    var i = 0
    while (i < list.length) {
      println(list(i))
      i += 1
    }
    for (i <- (0).to(list.length - 1)) {
      println(list(i))
    }
    for (i <- 0 to list.length - 1) {
      println(list(i))
    }
    for (element <- list) {
      println(element)
    }
    list.foreach(println)
  }

  private def greatestCommonDivisor(a: Int, b: Int): Int = if (b == 0) a else greatestCommonDivisor(b, a % b)

  @Test
  def testGcd(): Unit = {
    println(greatestCommonDivisor(18, 9))
  }
}
