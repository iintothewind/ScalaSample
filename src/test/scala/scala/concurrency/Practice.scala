package scala.concurrency

import com.google.common.collect.MapMaker
import org.junit.rules.TestWatcher
import org.junit.runner.Description
import org.junit.{Rule, Test}

import scala.actors.Actor
import scala.actors.threadpool.TimeUnit

/**
 * Actors should not block
 * Communicate with actors only via messages
 * Prefer immutable message
 * Make message self-contained
 *
 */
class Practice {
  @Rule
  def watcher = new TestWatcher {
    override def succeeded(description: Description): Unit = TimeUnit.SECONDS.sleep(1)
  }

  /**
   * never block actors from acting, unless it never receives any message
   * or it would miss receiving the incoming messages
   */
  @Test
  def avoidBlockingMainActor(): Unit = {
    val sillyActor = Actor.actor {
      def emoteLater(): Unit = {
        val mainActor = Actor.self

        /**
         * sleeping actor never receive a message, so it is ok to block
         */
        Actor.actor {
          TimeUnit.MILLISECONDS.sleep(100)
          mainActor ! "Emote"
        }
      }

      var emoted = 0
      emoteLater()

      Actor.loop {
        Actor.react {
          case "Emote" =>
            println("I'm acting!")
            emoted += 1
            if (emoted < 5) emoteLater()

          case msg => println(s"received: $msg")
        }
      }
    }

    sillyActor ! "Hi, Silly Actor"
  }

  /**
   * The metaphor of this store is that both Non-sharing model (Actor) and Sharing Locks model are tricky,
   * play with both means playing with most powerful shotgun that would shot yourself by mistake.
   */
  @Test
  def avoidSharingDataAndLocks(): Unit = {
    val map = new MapMaker().weakKeys().weakValues().makeMap[Int, String]()
    //Source.fromFile("pom.xml").getLines().zipWithIndex.foreach(line => map.put(line._2, line._1))
    // there should be a proer case to showcase this
    val readActor = Actor.actor {
      Actor.loop {
        Actor.react {
          case "start" => None
        }
      }
    }
  }


}
