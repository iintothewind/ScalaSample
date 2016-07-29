package fp.intro

import org.junit.Test

class LazinessSample {
  @Test
  def testLogicOperatorLaziness(): Unit = {
    val lazyAnd = false && {
      println("!!");
      true
    }
    val lazyOr = true || {
      println("!!");
      false
    }
    assert(lazyAnd != lazyOr)
  }

  @Test
  def testIfElseLaziness(): Unit = {
    val result = if (Nil.nonEmpty) sys.error("empty input") else "good"
  }

  def judge[A](condition: Boolean)(onTrue: => A)(onFalse: => A): A = if (condition) onTrue else onFalse

  @Test
  def testJudge(): Unit = {
    judge(Nil.isEmpty)(println("true"))(println("false"))
  }

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

  def maybeOnce(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

  @Test
  def testMaybeTwice(): Unit = {
    val x = maybeTwice(true, {
      println("evaluating") // evaluating would be printed twice
      42
    })
  }

  @Test
  def testMaybeOnce(): Unit = {
    val y = maybeOnce(true, {
      println("evaluating") // evaluating would be printed once
      42
    })
  }
}