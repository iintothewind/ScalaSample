package scala.objects

import org.junit.Test

import scala.collection.mutable

class ObjectSample {

  class CheckSumAccumulator {
    private var sum = 0

    def add(b: Byte): Unit = sum += b

    def checkSum(): Int = ~(sum & 0xFF) + 1

  }

  /**
   * companion object for class CheckSumAccumulator
   */
  object CheckSumAccumulator {
    private val cache = mutable.Map[String, Int]()

    def calculate(s: String): Int = {
      if (cache.contains(s)) {
        cache(s)
      }
      else {
        val acc = new CheckSumAccumulator
        for (c <- s) acc.add(c.toByte)
        val cs = acc.checkSum()
        cache += (s -> cs)
        cs
      }
    }

  }

  /**
   * standalone singleton object,
   * any standalone object with a main() method of the proper signature can be used as the entry point into an application
   */
  object Summer {


    import CheckSumAccumulator.calculate

    def sum(args: String*): Unit = {
      args.foreach(str => println(str + ":" + calculate(str)))
    }
  }

  /**
   * standalone singleton object extends App
   * just work as main(), but cannot use command line arguments, args[]
   */
  object MyApp extends App {

    import CheckSumAccumulator.calculate

    List("a", "b", "c").foreach(calculate)
  }

  @Test
  def instantiate(): Unit = {
    val acc = new CheckSumAccumulator
    val csa = new CheckSumAccumulator
  }

  @Test
  def singleton(): Unit = {
    import CheckSumAccumulator.calculate
    calculate("Every value is an object.")
    import Summer.sum
    sum("a", "b", "c")
  }

  abstract class Animal {
    def name: String = "Animal"
  }

  class Dog extends Animal {
    override def name: String = "Dog"
  }

  @Test
  def testDog(): Unit = {
    def dog = new Dog
    assert("Dog" == dog.name)
  }

}
