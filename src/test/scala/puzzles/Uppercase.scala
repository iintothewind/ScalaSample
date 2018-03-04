package puzzles

import org.junit.Test

/**
  * var MONTH = 12; var DAY = 24
  * var (HOUR, MINUTE, SECOND) = (12, 0, 0)
  * multiple-variable assignments are based on
  * pattern matching,
  * and within a pattern match, variables starting with an uppercase letter take on a special meaning:
  * they are stable identifiers.
  * Stable identifiers are intended for matching against constants
  */
class Uppercase {
  @Test
  def multipleUppercaseVarsAssign(): Unit = {
    var MONTH = 12;
    var DAY = 24
    //var (HOUR, MINUTE, SECOND) = (12, 0, 0) // Cannot resolve symbol
  }

  @Test
  def multipleUppercaseMatchSuccess(): Unit = {
    val MONTH = 12
    val DAY = 24
    val HOUR = 12;
    val MINUTE, SECOND = 0 // match constants
    var (HOUR, MINUTE, SECOND) = (12, 0, 0) // pattern match successful
    println(s"HOUR=$HOUR, MINUTE=$MINUTE, SECOND=$SECOND")
  }

  @Test(expected = classOf[MatchError])
  def multipleUppercaseMatchFail(): Unit = {
    val MONTH = 12
    val DAY = 24
    val HOUR = 12;
    val MINUTE, SECOND = 0 // match constants
    var (HOUR, MINUTE, SECOND) = (12, 1, 0) // pattern match error
    println(s"HOUR=$HOUR, MINUTE=$MINUTE, SECOND=$SECOND")
  }

}
