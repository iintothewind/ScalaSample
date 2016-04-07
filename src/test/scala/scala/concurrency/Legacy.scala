package scala.concurrency


import java.net.{UnknownHostException, InetAddress}

import org.junit.{Rule, Test}
import org.junit.rules.TestWatcher
import org.junit.runner.Description

import scala.actors.Actor
import scala.actors.threadpool.TimeUnit


class Legacy {
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


}
