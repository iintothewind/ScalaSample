package scala.basic

import org.junit.Test


class OperationsSample {

  @Test
  def testOperations(): Unit = {
    val sum = 1 + 2
    val sumMore = (1).+(2)
    val longSum = 1 + 2L
    val s = "Hello, World!"
    printf("s indexOf \'o\' == %s \n", s indexOf 'o')
    printf("s indexOf \'o\' == %s \n", s.indexOf('o', 5))
    printf("s.toLowerCase \n", s.toLowerCase)
  }

  @Test
  def testMoreOperations(): Unit = {
    val s = ""
    printf(" test s == %s \n", s.isEmpty)
    printf( """("he"+"llo") == "hello" is : %s""".stripMargin, ("he" + "llo") == "hello")
    val ss = "student"
    assert("Student" == ss.capitalize)
  }


}
