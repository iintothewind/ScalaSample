package basic.pars.basic

import org.junit.rules.TestWatcher
import org.junit.runner.Description
import org.junit.{Rule, Test}

import scala.actors.threadpool.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
http://docs.scala-lang.org/overviews/core/futures
  */
class Futures {

  @Rule
  def watcher: TestWatcher = new TestWatcher {
    override def succeeded(description: Description): Unit = TimeUnit.SECONDS.sleep(1)
  }

  @Test
  def testFuture(): Unit = {
    val msg = Future {
      TimeUnit.MILLISECONDS.sleep(10)
      "test"
    }.onComplete {
      case Success(s) => println(s)
      case Failure(e) => println(e.getMessage)
    }
  }


}
