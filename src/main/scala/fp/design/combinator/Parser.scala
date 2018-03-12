package fp.design.combinator

import scala.util.Either
import scala.language.{higherKinds, implicitConversions}

sealed case class ParseError()

sealed trait Parsers[Parser[+ _]] { self =>

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

  def produce[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] = map(produce(a, b))(f.tupled)

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(List()))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def repeat[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
}


