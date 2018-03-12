package fp.design.combinator

import scala.util.Either
import scala.language.{higherKinds, implicitConversions}

sealed case class ParseError()

sealed case class SimpleParser[+A](a: A)

sealed trait SimpleParsers[SimpleParser[+ _]] { self =>

  case class SimpleParserOps[A](a: SimpleParser[A]) {
    def ||[B >: A](b: => SimpleParser[B]): SimpleParser[B] = self.or(a, b)

    def or[B >: A](b: => SimpleParser[B]): SimpleParser[B] = self.or(a, b)

    def **[B](b: => SimpleParser[B]): SimpleParser[(A, B)] = self.produce(a, b)

    def produce[B](b: => SimpleParser[B]): SimpleParser[(A, B)] = self.produce(a, b)
  }

  implicit def string(s: String): SimpleParser[String]

  implicit def operators[A](p: SimpleParser[A]): SimpleParserOps[A] = SimpleParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => SimpleParser[String]): SimpleParserOps[String] = SimpleParserOps(f(a))

  def or[A](l: SimpleParser[A], r: => SimpleParser[A]): SimpleParser[A]

  def produce[A, B](a: SimpleParser[A], b: => SimpleParser[B]): SimpleParser[(A, B)]

  def flatMap[A, B](a: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B]

  def map[A, B](a: SimpleParser[A])(f: A => B): SimpleParser[B]

  def map2[A, B, C](a: SimpleParser[A], b: => SimpleParser[B])(f: (A, B) => C): SimpleParser[C] = map(produce(a, b))(f.tupled)

  def char(c: Char): SimpleParser[Char] = map(string(c.toString))(_.charAt(0))

  def succeed[A](a: A): SimpleParser[A] = map(string(""))(_ => a)

  def many[A](p: SimpleParser[A]): SimpleParser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(List()))

  def listOfN[A](n: Int, p: SimpleParser[A]): SimpleParser[List[A]] = if (n <= 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def repeat[A](p: SimpleParser[A]): SimpleParser[List[A]] = map2(p, many(p))(_ :: _)

  def slice[A](p: SimpleParser[A]): SimpleParser[String]

  def run[A](p: SimpleParser[A])(input: String): Either[ParseError, A]
}


