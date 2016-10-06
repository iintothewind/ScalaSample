package puzzles

import org.junit.Test
import puzzles.MissingList.sumSizes
import puzzles.MissingList.sumSizesAfterTransformCollectionsIntoKnowType

object MissingList {
  def sumSizes(collections: Iterable[Iterable[_]]): Int =
    collections.map(_.size).sum

  def sumSizesAfterTransformCollectionsIntoKnowType(collections: Iterable[Iterable[_]]): Int =
    collections.toSeq.map(_.size).sum

}

class MissingList {
  /**
    * To understand what is going on, you need to recall another feature of the Scala collections library:
    * its operations generally preserve the input collection type!
    * Developers coming from a Java background might expect that
    * transforming an Iterable would produce a result that conforms to the Iterable interface,
    * but could be any underlying implementation type.
    * The Scala collections go one step further, though, and return an Iterable with the same type as the input type.
    * This means the result type of the following statement is also a set and thus cannot contain multiple instances of the same item:
    * Set(List(1, 2), Set(3, 4)).map(_.size)
    * As a result, this intermediate value contains only one element,This behavior of sets explains the observed result
    */
  @Test
  def runPuzzle(): Unit = {
    assert(sumSizes(List(Set(1, 2), List(3, 4))) == 4)
    assert(sumSizes(Set(List(1, 2), Set(3, 4))) == 2)
  }

  @Test
  def avoidSurprise(): Unit = {
    assert(sumSizesAfterTransformCollectionsIntoKnowType(List(Set(1, 2), List(3, 4))) == 4)
    assert(sumSizesAfterTransformCollectionsIntoKnowType(Set(List(1, 2), Set(3, 4))) == 4)
  }
}
