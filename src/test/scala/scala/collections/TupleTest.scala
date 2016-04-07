package scala.collections

import org.junit.Test

class TupleTest {
  @Test
  def useTuple(): Unit = {
    val tuple2 = (99, "balloon")
    println(tuple2.getClass.getName)
    println(tuple2._1)
    println(tuple2._2)

    val tuple3 = ("1", 2, 3d)
    println(tuple3.getClass.getName)
    println(tuple3._1)
    println(tuple3._2)
    println(tuple3._3)
  }


  @Test
  def returnMultiValues(): Unit = {
    // the return type uses the syntactic sugar, not Tuple2[String, Int]
    def longestWord(words: Array[String]): (String, Int) = {
      val longest = words.max(Ordering.fromLessThan[String]((left, right) => if (left.length == right.length) left < right else left.length < right.length))
      (longest, longest.length)
    }
    println(longestWord("The quick brown fox jump over the lazy dog".split("[ ,.!;]+")))
  }

  @Test
  def assignMultiValues(): Unit = {
    val pair = ("fox", 3)
    val (animal, number) = pair
    assert("fox" == animal)
    assert(3 == number)
    //a pitfall, never leave off the parentheses
    val notAnimal, notNumber = pair
    //or the syntax gives multiple definitions of the same expression
    assert(pair == notAnimal)
    assert(pair == notNumber)
  }


}
