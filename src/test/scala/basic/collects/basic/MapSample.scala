package basic.collects.basic

import java.util

import org.junit.Test

import scala.collection.immutable.TreeMap
import scala.collection.{immutable, mutable}
import scala.io.Source

class MapSample {
  @Test
  def immutableMap(): Unit = {
    val romanNumeral = immutable.Map(1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V")
    println(romanNumeral(4))
  }

  @Test
  def mutableMap(): Unit = {
    val treasureMap = mutable.Map[Int, String]()
    treasureMap += (1 -> "Go to island.")
    treasureMap += (2 -> "Find big X on ground.")
    treasureMap += (3 -> "Dig.")
    println(treasureMap(2))
    assert(treasureMap(3) == "Dig.")
  }

  @Test
  def mutableValueSet(): Unit = {
    val map = mutable.Map.empty[Int, String]
    map(1) = "one"
    map(2) = "two"
    map(3) = "three"
    assert("one" == map(1))
    assert("two" == map(2))
    assert("three" == map(3))
  }

  @Test
  def iterateJavaMap(): Unit = {
    val map: java.util.Map[Int, String] = new util.HashMap[Int, String]()
    import scala.collection.JavaConversions._
    map.put(1, "one")
    map.put(2, "two")
    map.put(3, "three")
    map.foreach(pair => println(pair._1 + " -> " + pair._2))
  }

  @Test
  def testCountWords(): Unit = {
    def countWords(text: String): Map[String, Int] = {
      val counts = mutable.Map.empty[String, Int]
      for (word <- text.split("[ ,.!;]+")) if (counts.contains(word)) {
        val count = counts(word) + 1
        counts += (word -> count)
      } else {
        counts += (word -> 1)
      }
      counts.toMap
    }
    assert(Map("See" -> 1, "Spot" -> 2, "Run" -> 3) == countWords("See Spot Run. Run, Spot. Run!"))
    assert(Map("Run" -> 3, "Spot" -> 2, "See" -> 1) == countWords("See Spot Run. Run, Spot. Run!"))
  }


  @Test
  def testWordCounting(): Unit = {
    Source.fromFile("README.md").getLines().flatMap(_.split("[ ,.!;]+"))
      .foldLeft(Map.empty[String, Int])((map, word) => map.+(word -> (map.getOrElse(word, 0) + 1)))
      .toList.sortBy(_._2)(Ordering[Int].reverse).foreach(println)
  }

  @Test
  def sortedMap(): Unit = {
    val treeMap = TreeMap(1 -> "HK", 2 -> "TW", 3 -> "MC")(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = y - x
    })
    treeMap.foreach(pair => println(pair._1 + " -> " + pair._2))
  }

  @Test
  def syntacticSugar(): Unit = {
    var capital = Map("US" -> "Washington", "France" -> "Paris")
    capital += ("Japan" -> "Tokyo")
    capital.foreach(pair => println(pair._1 + " -> " + pair._2))
  }

}
