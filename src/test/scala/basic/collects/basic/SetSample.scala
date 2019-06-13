package basic.collects.basic

import org.junit.{Assert, Test}

import scala.collection.immutable.TreeSet
import scala.collection.{immutable, mutable}


class SetSample {
  @Test
  def immutableSet(): Unit = {
    val jetSet = immutable.Set("Boeing", "Airbus")
    val jets = jetSet + "Cessna"
    Assert.assertTrue(jets.contains("Cessna"))
    Assert.assertEquals(3, jets.size)
  }

  @Test
  def immutableHashSet(): Unit = {
    val hashSet = immutable.HashSet("Tomatoes", "Chilies")
    println(hashSet + "Coriander")
  }

  @Test
  def testDistinct(): Unit = {
    val base = immutable.Set.empty[String]
    val words = "See Spot Run. Run, Spot. Run!".split("[ !,.]+")

    def distinct(words: Array[String]): immutable.Set[String] = words.foldLeft(immutable.Set.empty[String])(_ + _)
    assert(Set("See", "Spot", "Run") == distinct(words))
    assert(Set("See", "Spot", "Run") == base ++ words)
  }

  @Test
  def mutableSet(): Unit = {
    val jetSet = mutable.Set("Boeing", "Airbus")
    jetSet += "Lear"
    jetSet += "Lear"
    Assert.assertFalse(jetSet.contains("Cessna"))
    Assert.assertEquals(3, jetSet.size)
  }

  @Test
  def remove(): Unit = {
    val nums = Set(1, 2, 3)
    assert(Set(1, 2) == nums - 3)
  }

  @Test
  def addMulti(): Unit = {
    val nums = Set(1, 2, 3)
    assert(Set(1, 2, 3, 4, 5) == nums ++ List(4, 4, 5, 5))
  }

  @Test
  def removeMulti(): Unit = {
    val nums = Set(1, 2, 3)
    assert(Set(1) == nums -- List(2, 2, 3, 3))
  }

  @Test
  def intersect(): Unit = {
    val nums1 = Set(1, 2, 3)
    val nums2 = Set(2, 3, 4, 5)
    assert(Set(2, 3) == (nums1 & nums2))
  }

  @Test
  def mutableOperations(): Unit = {
    val words = mutable.Set.empty[String]
    words += "java"
    words -= "java"
    words ++= List("do", "re", "mi")
    words --= List("do", "re", "mi")
    words += "scala"
    words.clear()
    assert(0 == words.size)
  }

  @Test
  def treeSet(): Unit = {
    val treeSet = TreeSet(9, 8, 5, 4, 8, 7, 6, 2, 3, 1)(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = y - x
    })
    println(treeSet)
  }

  @Test
  def syntacticSugar(): Unit = {
    val people = Set.empty[String]
    //+= is not supported by immutable set
    //people += "Ada"
    var country = Set.empty[String]
    country += "Agron"
    country += "Belish"
    assert(Set("Agron", "Belish") == country)
  }
}
