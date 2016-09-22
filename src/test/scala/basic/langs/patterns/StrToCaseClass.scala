package basic.langs.patterns

import org.junit.Test

class StrToCaseClass {


  case class Data(a: Int, b: Int, c: Int, d: Int, e: Int)


  @Test
  def functionCurried(): Unit = {
    assert(Data(5, 4, 3, 2, 1) == Data.curried(5)(4)(3)(2)(1))
  }


  @Test
  def curryImpl(): Unit = {
    val arr = "1\t2\t3\t4\t5".split('\t')
    val myData = arr.foldLeft[Any](Data.curried){(f, s) => f.asInstanceOf[Any => Any](s.toInt)}.asInstanceOf[Data]
    assert(Data(1, 2, 3, 4, 5) == myData)
  }


}
