package fp.combinator

import fp.combinator.SimpleParser._
import org.junit

@junit.Ignore
class SimpleParserSample {

  @junit.Test
  def testRun(): Unit = {
    println(SimpleParser.run("a")("a"))
  }

  @junit.Test
  def testChar(): Unit = {
    SimpleParser.run(char('a'))("a").ensuring(_ == Right('a'))
    SimpleParser.run(char('a'))("ab").ensuring(_ == Right('a'))
    SimpleParser.run(char('a'))("bc").ensuring(_ match {
      case Left(ParseError((List((Location("bc", 0), _))))) => true
      case _ => false
    })
  }

  @junit.Test
  def testString(): Unit = {
    SimpleParser.run("a")("aa").ensuring(_ == Right("a"))
  }

  @junit.Test
  def testMany(): Unit = {
    SimpleParser.run(many("aa" or "bb" or "dd" or "ee"))("aabbccddee").ensuring(_ == Right(List("aa", "bb")))
  }

}
