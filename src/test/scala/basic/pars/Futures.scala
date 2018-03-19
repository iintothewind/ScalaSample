package basic.pars

import java.util.concurrent.TimeUnit

import org.junit.rules.TestWatcher
import org.junit.runner.Description
import org.junit.{Rule, Test}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * http://docs.scala-lang.org/overviews/core/futures
  */
class Futures {

  @Rule
  def watcher: TestWatcher = new TestWatcher {
    override def starting(description: Description): Unit = println(s"testing method: ${description.getMethodName}()")

    override def succeeded(description: Description): Unit = TimeUnit.SECONDS.sleep(1)
  }

  @Test
  def testFirstCompletedOf(): Unit = {
    val msg = Future {
      TimeUnit.MILLISECONDS.sleep(100)
      "msg1 test"
    }

    val msg2 = Future {
      TimeUnit.MILLISECONDS.sleep(90)
      throw new RuntimeException("sth wrong")
    }

    Future.firstCompletedOf(Seq(msg, msg2)).onComplete {
      case Success(s) => println(s)
      case Failure(e) => println(e.getMessage)
    }
  }


}
