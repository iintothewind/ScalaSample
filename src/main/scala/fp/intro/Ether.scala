package fp.intro

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
