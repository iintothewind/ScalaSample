package scala.traits

import org.junit.Test

trait User {
  def name: String
}

trait Tweeter {
  user: User =>
  def tweet(msg: String): Unit = {
    println(s"$name: $msg")
  }
}

/**
 * Error:(16, 21) illegal inheritance;
 * self-type scala.traits.Right does not conform to scala.traits.Tweeter's selftype scala.traits.Tweeter with scala.traits.User
 * trait Right extends Tweeter  {
 **/
class Right extends Tweeter with User {
  override def name: String = "Mr.right"
}

class SelfTypeAnnotationSample {
  @Test
  def testSelfTypeAnnotation(): Unit = {
    val right = new Right
    right.tweet("Hello")
  }
}
