package basic.langs.simple

import org.junit.Test

import scala.io.Source

class ReadSample {

  @Test
  def readSource(): Unit = {
    Source.fromFile(getClass.getResource("/country.csv").toURI).getLines().foreach(println)
  }

}
