package basic.langs.structure

import java.io._
import java.net.{MalformedURLException, URL}

import org.junit.Test

import scala.io.Source

class ErrorHandling {

  def half(number: Int): Int = if (number % 2 == 0) number / 2 else throw new RuntimeException("number must be even.")

  @Test(expected = classOf[RuntimeException])
  def throwException(): Unit = {
    half(3)
  }

  @Test
  def catchException(): Unit = {
    val reader = Source.fromFile(new File("./build.sbt")).bufferedReader()
    try {
      println(reader.readLine())
    } catch {
      case e: FileNotFoundException => println(e.getMessage)
      case e: IOException => throw e
    } finally {
      reader.close()
    }
  }

  def urlFor(path: String): URL = {
    try {
      new URL(path)
    } catch {
      case e: MalformedURLException => new URL("http://www.google.com")
    }
  }

  @Test
  def catchAndYield(): Unit = {
    println(urlFor(""))
  }

}
