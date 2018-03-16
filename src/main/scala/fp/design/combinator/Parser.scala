package fp.design.combinator

import scala.util.Either
import scala.language.{higherKinds, implicitConversions}

sealed case class ParseError(stack: List[(Location, String)] = List()) {
  def push(location: Location, msg: String): ParseError = copy(stack = (location, msg) :: stack)

  def latest: Option[(Location, String)] = stack.lastOption

  def latestLocation: Option[Location] = latest.map(_._1)

  def label[A](s: String): ParseError = ParseError(latestLocation.map((_, s)).toList)
}

sealed case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def currentLine: String = if (input.length > 1) input.lines.drop(line - 1).next else ""

  def columnCaret: String = (" " * (col - 1)) + "^"

}

trait Parsers[Parser[+ _]] { self =>

  case class ParserOps[A](a: Parser[A]) {
    def ||[B >: A](b: => Parser[B]): Parser[B] = self.or(a, b)

    def or[B >: A](b: => Parser[B]): Parser[B] = self.or(a, b)

    def **[B](b: => Parser[B]): Parser[(A, B)] = self.produce(a, b)

    def produce[B](b: => Parser[B]): Parser[(A, B)] = self.produce(a, b)
  }

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def or[A](l: Parser[A], r: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def produce[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)] = flatMap(a)(a => map(b)(b => (a, b)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f.andThen(succeed))

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] = map(produce(a, b))(f.tupled)

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def succeed[A](a: A): Parser[A]

  // to avoid stackoverflow, do not use: or(map2(p, many(p))(_ :: _), succeed(List()))
  def many[A](p: Parser[A]): Parser[List[A]]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def repeat[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
}


