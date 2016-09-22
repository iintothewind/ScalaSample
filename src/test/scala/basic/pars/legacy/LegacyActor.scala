package basic.pars.legacy

import java.net.{InetAddress, UnknownHostException}

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
class LegacyActor {
  @Rule
  def watcher = new TestWatcher {
    override def succeeded(description: Description): Unit = TimeUnit.SECONDS.sleep(1)
  }

  object SillyActor extends Actor {
    override def act(): Unit = {
      for (i <- 1 to 5) {
        println("I'm acting!")
      }
    }
  }

  @Test
  def startActor(): Unit = {
    SillyActor.start()
  }

  object SeriousActor extends Actor {
    override def act(): Unit = {
      for (i <- 1 to 5) {
        println("To be or not to be.")
      }
    }
  }

  @Test
  def startBoth(): Unit = {
    SillyActor.start()
    SeriousActor.start()
  }

  @Test
  def echo(): Unit = {
    val echoActor = Actor.actor {
      while (true) {
        Actor.receive {
          case msg => println(s"received: $msg")
        }
      }
    }
    echoActor ! "Hi there"
    echoActor ! 15
  }

  @Test
  def intProcess(): Unit = {
    val intActor = Actor.actor {
      Actor.receive {
        case x: Int => println(s"Got an int: $x")
      }
    }

    intActor ! "hello"
    intActor ! math.Pi
    intActor ! 12
  }

  @Test
  def nativeThreadAsAcotrs(): Unit = {
    Actor.self ! "hello"
    Actor.self ! "hello, Native Thread"
    Actor.self.receive { case msg => println(s"msg: $msg") }
    Actor.self.receive { case msg => println(s"msg: $msg") }
    // no further msg coming, the third one will be blocked, till time out
    Actor.self.receiveWithin(100) { case msg => println(s"msg: $msg") }
  }

  object NameResolver extends Actor {
    def getIp(name: String): Option[InetAddress] = {
      try {
        Some(InetAddress.getByName(name))
      } catch {
        case _: UnknownHostException => None
      }
    }

    override def act(): Unit = {
      // Because the react method does not need to return,
      // the implementation does not need to preserve the call stack of the current thread
      Actor.react {
        case (hostname: String, actor: Actor) => actor ! getIp(hostname); act()
        case "EXIT" => println("Name resolver exiting.")
        case msg => println(s"Unhandled message: $msg"); act()

      }
    }
  }

  @Test
  def ipAddress(): Unit = {
    NameResolver.start()
    NameResolver !("localhost", Actor.self)
    NameResolver ! "EXIT"
    NameResolver ! "jira.int.corp.sun"
    Actor.self.receiveWithin(100) { case ip => println(s"ip: $ip") }
  }

  @Test
  def loopReact(): Unit = {
    val loopActor = Actor.actor {
      Actor.loop {
        Actor.react {
          case msg => println(s"msg: $msg")
        }
      }
    }
    loopActor ! "Hello"
    loopActor ! "Hello, Anybody there?"
    loopActor ! "Hello, Actor"
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
