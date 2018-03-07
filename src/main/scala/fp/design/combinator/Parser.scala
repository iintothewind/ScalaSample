package fp.design.combinator

import scala.util.Either
import scala.language.{higherKinds, implicitConversions}

sealed case class ParseError()

sealed case class SimpleParser[+A](a: A)

sealed trait SimpleParsers[ParseError, SimpleParser[+ _]] { self =>

  case class SimpleParserOps[A](p: SimpleParser[A]) {
    def ||[B >: A](p2: SimpleParser[B]): SimpleParser[B] = self.or(p, p2)

    def or[B >: A](p2: => SimpleParser[B]): SimpleParser[B] = self.or(p, p2)
  }

  implicit def string(s: String): SimpleParser[String]

  implicit def operators[A](p: SimpleParser[A]): SimpleParserOps[A] = SimpleParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => SimpleParser[String]): SimpleParserOps[String] = SimpleParserOps(f(a))

  def or[A](l: SimpleParser[A], r: SimpleParser[A]): SimpleParser[A]

  def char(c: Char): SimpleParser[Char]

  def run[A](p: SimpleParser[A])(input: String): Either[ParseError, A]

  def repeat[A](n: Int, p: SimpleParser[A]): SimpleParser[A]

}
