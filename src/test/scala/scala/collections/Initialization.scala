package scala.collections

import org.junit.Test

import scala.collection.{immutable, mutable}
import scala.collection.immutable.TreeSet

class Initialization {
  @Test
  def explicitType(): Unit = {
    val colors = List[String]("blue", "green", "red")
  }

  @Test
  def convert(): Unit = {
    val colors = List[String]("blue", "green", "red")
    val treeSet = TreeSet.empty[String] ++ colors
    val list = treeSet.toList
    val array = treeSet.toArray
  }

  @Test
  def convertBetweenMutableAndImmutable(): Unit = {
    val colors = List("blue", "green", "red")
    val immutableColors = immutable.Set.empty[String] ++ colors
    val mutableColors = mutable.Set.empty[String] ++ colors
    val anotherImmutableColors = immutable.Set.empty[String] ++ mutableColors
    val alternativeImmutableColors = mutableColors.toSet
    assert(immutableColors == mutableColors)
    assert(immutableColors == anotherImmutableColors)
    assert(immutableColors == alternativeImmutableColors)
  }


}
