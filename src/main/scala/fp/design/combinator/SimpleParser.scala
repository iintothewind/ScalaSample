package fp.design.combinator

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions


sealed case class ParserState(location: Location) {
  def advanceBy(numChars: Int): ParserState = copy(location = location.copy(offset = location.offset + numChars))

  def input: String = location.input.substring(location.offset)

  def slice(n: Int): String = location.input.substring(location.offset, location.offset + n)
}

sealed trait ParseResult[+A] {
  def extract: Either[ParseError, A] = this match {
    case Failure(e, _) => Left(e)

    case Success(a, _) => Right(a)
  }

  def uncommit: ParseResult[A] = this match {
    case Failure(e, true) => Failure(e, isCommitted = false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): ParseResult[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def mapError(f: ParseError => ParseError): ParseResult[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }

  def advanceSuccess(n: Int): ParseResult[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }

}

sealed case class Success[+T](get: T, length: Int) extends ParseResult[T]

sealed case class Failure(get: ParseError, isCommitted: Boolean) extends ParseResult[Nothing]

object SimpleParser extends Parsers[Parser] {
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) {
        return i
      } else {
        i += 1
      }
    }
    if (s1.length - offset >= s2.length) {
      -1
    } else {
      s1.length - offset
    }
  }

  override implicit def string(s: String): Parser[String] = {
    val msg = s"'$s'"
    state => {
      firstNonmatchingIndex(state.location.input, s, state.location.offset) match {
        case -1 => Success(s, s.length)
        case i@_ => Failure(state.location.advanceBy(i).toError(msg), i != 0)
      }
    }
  }

  override def or[A](l: Parser[A], r: => Parser[A]): Parser[A] = {
    state =>
      l(state) match {
        case Failure(_, false) => r(state)
        case otherwise => otherwise
      }
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
    state =>
      f(state) match {
        case Success(a, n) => f(a)(state.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case failure@Failure(_, _) => failure
      }
  }

  override def slice[A](p: Parser[A]): Parser[String] = {
    state =>
      p(state) match {
        case Success(_, n) => Success(state.slice(n), n)
        case failure@Failure(_, _) => failure
      }
  }

  override def many[A](p: Parser[A]): Parser[List[A]] = {
    state => {
      val buf = ListBuffer.empty[A]

      def go(p: Parser[A], offset: Int): ParseResult[List[A]] = {
        p(state.advanceBy(offset)) match {
          case Success(a, n) => buf += a; go(p, offset + n)
          case failure@Failure(e, true) => failure
          case Failure(e, _) => Success(buf.toList, offset)
        }
      }

      go(p, 0)
    }
  }

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(ParserState(Location(input))).extract
}

