package scala.basic

import org.junit.Test

import scala.io.Source

class IOSample {

  @Test
  def readSource(): Unit = {
    Source.fromFile(getClass.getResource("/country.csv").toURI).getLines().foreach(println)
  }

}
