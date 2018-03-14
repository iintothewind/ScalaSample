package fp.design.combinator

import fp.design.combinator.SimpleParser._
import org.junit

class SimpleParserSample {

  @junit.Test
  def testRun(): Unit = {
    println(SimpleParser.run(many("a"))("aaaaa"))
  }

}
