package scala.basic

import java.io.FileNotFoundException

import org.junit.Test

import scala.io.Source
import scala.util.Try

class ErrorHandling {

  @Test
  def testRead(): Unit = {
    Source.fromFile(sys.props.get("user.dir").getOrElse(".").concat("/pom.xml")).getLines().foreach(println)
  }

  @Test(expected = classOf[FileNotFoundException])
  def testReadNonExisting(): Unit = {
    Source.fromFile("any").getLines().foreach(println)
  }

  @Test
  def testSafeReadNonExisting(): Unit = {
    Try(Source.fromFile("any").getLines()).getOrElse(Iterator.empty).foreach(println)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testOptionToEither(): Unit = {
    Option(0).filter(_ != 0).toRight(new IllegalArgumentException) match {
      case Left(e) => throw e
      case Right(v) => println(v)
    }
  }
}
