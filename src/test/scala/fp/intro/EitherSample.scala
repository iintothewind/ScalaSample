package fp.intro

import org.junit.Test

sealed trait Ether[+E, +A] {
  def isLeft: Boolean

  def isRight: Boolean

  def toOption: Option[A] = this match {
    case Lft(_) => None
    case Rht(a) => Some(a)
  }

  def OrElse[UE >: E, UA >: A](that: => Ether[UE, UA]): Ether[UE, UA] = this match {
    case Lft(_) => that
    case Rht(_) => this
  }

  def flatMap[UE >: E, B](f: A => Ether[UE, B]): Ether[UE, B] = this match {
    case Lft(e) => Lft(e)
    case Rht(a) => try f(a) catch {
      case e: Exception => Lft(e.asInstanceOf[UE])
    }
  }

  def map[B](f: A => B): Ether[E, B] = this match {
    case Lft(e) => Lft(e)
    case Rht(a) => try Rht(f(a)) catch {
      case e: Exception => Lft(e.asInstanceOf[E])
    }
  }

  def map2[UE >: E, B, C](that: Ether[UE, B])(f: (A, B) => C): Ether[UE, C] = (this, that) match {
    case (Rht(a), Rht(b)) => try Rht(f(a, b)) catch {
      case e: Exception => Lft(e.asInstanceOf[UE])
    }
    case (_, Lft(e)) => Lft(e)
    case (Lft(e), _) => Lft(e)
  }

}

sealed case class Lft[+E, +A](e: E) extends Ether[E, A] {
  override def isLeft: Boolean = true

  override def isRight: Boolean = false
}

sealed case class Rht[+E, +A](a: A) extends Ether[E, A] {
  override def isLeft: Boolean = false

  override def isRight: Boolean = true
}

object Ether {

  def fromOption[E, A](opt: Option[A]): Ether[E, A] = opt match {
    case Some(a) => Rht(a)
    case None => Lft(Void.TYPE.asInstanceOf[E])
  }

  def attempt[E, A](a: => A): Ether[E, A] = try Rht(a) catch {
    case e: Exception => Lft(e.asInstanceOf[E])
  }
}

class EitherSample {
  @Test
  def testFromOption(): Unit = {
    assert(Ether.fromOption(None).isLeft)
    assert(Rht("admin") == Ether.fromOption(Some("admin")))
  }

  @Test
  def testToOption(): Unit = {
    assert(Lft(new IllegalArgumentException("a")).toOption.isEmpty)
    assert(Rht("admin").toOption.contains("admin"))
  }

  @Test
  def testFlatMap(): Unit = {
    assert(Lft(new IllegalArgumentException("a")).flatMap(Rht(_)).isLeft)
    assert(Rht("12") == Rht(12).flatMap(x => Rht(x.toString)))
  }

  @Test
  def testMap(): Unit = {
    assert(Lft[Exception,String](new IllegalArgumentException("a")).map(_.toInt).isLeft)
    assert(Rht[Exception,String]("a").map(_.toInt).isLeft)
    assert(Rht(12) == Rht("12").map(_.toInt))
  }

  @Test
  def testMap2(): Unit = {
    assert(Lft[Exception,Int](new IllegalArgumentException("a")).map2(Rht(2))(_ + _).isLeft)
    assert(Rht(1).map2(Lft[Exception,Int](new IllegalArgumentException("b")))(_ + _).isLeft)
    assert(Rht(12) == Rht("1").map2(Rht("2"))((a, b) => a.concat(b).toInt))
  }

}