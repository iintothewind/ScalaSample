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
    case ParseFailure(e, _) => Left(e)

    case ParseSuccess(a, _) => Right(a)
  }

  def uncommit: ParseResult[A] = this match {
    case ParseFailure(e, true) => ParseFailure(e, isCommitted = false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): ParseResult[A] = this match {
    case ParseFailure(e, c) => ParseFailure(e, c || isCommitted)
    case _ => this
  }

  def mapError(f: ParseError => ParseError): ParseResult[A] = this match {
    case ParseFailure(e, c) => ParseFailure(f(e), c)
    case _ => this
  }

  def advanceSuccess(n: Int): ParseResult[A] = this match {
    case ParseSuccess(a, m) => ParseSuccess(a, n + m)
    case _ => this
  }

}

sealed case class ParseSuccess[+T](get: T, length: Int) extends ParseResult[T]

sealed case class ParseFailure(get: ParseError, isCommitted: Boolean) extends ParseResult[Nothing]

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
        case -1 => ParseSuccess(s, s.length)
        case i@_ => ParseFailure(state.location.advanceBy(i).toError(msg), i != 0)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] = _ => ParseSuccess(a, 0)

  override def or[A](l: Parser[A], r: => Parser[A]): Parser[A] = {
    state =>
      l(state) match {
        case ParseFailure(_, false) => r(state)
        case otherwise => otherwise
      }
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
    state =>
      p(state) match {
        case ParseSuccess(a, n) => f(a)(state.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case failure@ParseFailure(_, _) => failure
      }
  }

  override def slice[A](p: Parser[A]): Parser[String] = {
    state =>
      p(state) match {
        case ParseSuccess(_, n) => ParseSuccess(state.slice(n), n)
        case failure@ParseFailure(_, _) => failure
      }
  }

  override def many[A](p: Parser[A]): Parser[List[A]] = {
    state => {
      val buf = ListBuffer.empty[A]

      def go(p: Parser[A], offset: Int): ParseResult[List[A]] = {
        p(state.advanceBy(offset)) match {
          case ParseSuccess(a, n) => buf += a; go(p, offset + n)
          case failure@ParseFailure(e, true) => failure
          case ParseFailure(e, _) => ParseSuccess(buf.toList, offset)
        }
      }

      go(p, 0)
    }
  }

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(ParserState(Location(input))).extract
}

