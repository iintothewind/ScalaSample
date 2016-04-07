package scala.collections

import org.junit.Test

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class SequenceTest {
  @Test
  def list(): Unit = {
    val colors = List("red", "blue", "green")
    assert("red" == colors.head)
    assert(List("blue", "green") == colors.tail)
  }

  @Test
  def array(): Unit = {
    // Arrays allow you to hold a sequence of elements and efficiently access an
    // element at an arbitrary position
    val five = new Array[Int](5)
    val assigned = Array(5, 4, 3, 2, 1)
    assigned(0) = assigned(4)
    assert(Array(1, 4, 3, 2, 1).zip(assigned).forall(pair => pair._1 == pair._2))
  }

  @Test
  def listBuffer(): Unit = {
    // ListBuffer is a mutable object
    // You append elements with the += operator, and prepend them with the +=: operator
    val listBuffer = new ListBuffer[Int]
    listBuffer += 1
    listBuffer += 2
    3 +=: listBuffer
    assert(List(3, 1, 2) == listBuffer.toList)
  }

  @Test
  def arrayBuffer(): Unit = {
    val arrayBuffer = new ArrayBuffer[Int]()
    arrayBuffer += 12
    arrayBuffer += 15
    9 +=: arrayBuffer
    assert(Array(9, 12, 15).zip(arrayBuffer.toArray[Int]).forall(pair => pair._1 == pair._2))
  }

  @Test
  def stringOps(): Unit = {
    def isNumberic(s: String): Boolean = s.forall(_.isDigit)
    assert(isNumberic("123"))
  }

}
