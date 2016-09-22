package basic.langs.simple

import org.junit.Test

class StringLiteral {

  @Test
  def quoteRawString(): Unit = {
    println( """
               |Welcome to Ultamix 3000.
               |Type "HELP" for help.""".stripMargin)
  }
}
