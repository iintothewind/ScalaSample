package basic.objects.generics

import org.junit.Test

class TypeParameterization {

  class NonvariantCell[T](init: T) {
    private[this] var current = init

    def cur = current

    def cur_=(c: T): Unit = current = c
  }

  @Test
  def testNonvariant(): Unit = {
    val c1 = new NonvariantCell[String]("abc")
    // compilation error: Expression of type Cell[String] doesn't confirm to expected type Cell[Any]
    // val c2:NonvariantCell[Any] = c1
    // c2.cur = 1
    // val s:String=c1.cur
  }

  @Test
  def testArrayCovariant(): Unit = {
    val a1 = Array("abc")
    // compilation error: Expression of type Array[String] doesn't conform to expected type Array[Any]
    // val a2:Array[Any] = a1
  }





}
