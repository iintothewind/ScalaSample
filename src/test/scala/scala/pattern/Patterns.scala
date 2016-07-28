package scala.pattern


import org.junit.Test

import scala.math.{E, Pi}

class Patterns {
  def wildcardPattern(expr: Expr): Unit = expr match {
    case BinOp(_, left: Num, right: Num) => println(expr + " is a binary operation.")
    // variable name matters for pattern match, leftStr and rightStr cannot be changed to left and right
    case BinOp(_, leftStr: Str, rightStr: Str) => println(expr + " is a binary operation for Str.")
    case UnOp(_, _) => println(expr + " is a unary operation.")
    case _ =>
  }

  def constantPattern(any: Any): String = any match {
    case 5 => "five"
    case true => "true"
    case "hello" => "hello"
    case Nil => "empty list"
    case Pi => "Pi = " + Pi
    case E => "E = " + E
    case _ => "something else"
  }


  def variablePattern(any: Any): String = any match {
    case 0 => "zero"
    case somethingElse => "non-zero: " + somethingElse
  }

  def constructorPattern(expr: Expr): String = expr match {
    case UnOp("-", UnOp("-", e)) => e.toString
    case BinOp("+", Num(e), Num(_)) => e.toString
    case BinOp("+", Str(e), Str(_)) => e.toString
    case BinOp("*", Num(e), Num(_)) => e.toString
    case other => other.toString
  }

  def constructorPatternWithGuard(expr: Expr): String = expr match {
    case UnOp("-", UnOp("-", e)) => e.toString
    case BinOp("+", Num(a), Num(b)) if a == b => Num(a * 2).toString
    case BinOp("+", Str(a), Str(b)) if a == b => Str(a * 2).toString
    case BinOp("*", Num(a), Num(b)) if a == 0 || b == 0 => Num(0D).toString
    case other => other.toString
  }


  def sequencePattern(seq: List[Int]): String = seq match {
    case List(0, _*) => "list starts with 0"
    case other => other.toString()
  }

  def sequencePatternWithGuard(seq: List[Int]): String = seq match {
    case List(0, _*) if seq.forall(item => item >= 0) => "list starts with 0, and all elements are greater or equal than zero"
    case other => other.toString()
  }


  def tuplePattern(any: Any): String = any match {
    case (a, "a", c) => "Tuple3 :" + (a, "a", c).toString()
    case (a, b, c, d) => "Tuple4 :" + (a, b, c, d).toString()
    case (1, b, c, d, e) => "Tuple5 :" + (1, b, c, d, e).toString()
    case other => other.toString
  }

  def tuplePatternWithGuard(any: Any): String = any match {
    case (a, "a", c) if a.isInstanceOf[String] && c.isInstanceOf[String] => a + "a" + c
    case (a, b, c, d) if a.isInstanceOf[Int] && b.isInstanceOf[Int] && c.isInstanceOf[Int] && d.isInstanceOf[Int] => (a.asInstanceOf[Int] + b.asInstanceOf[Int] + c.asInstanceOf[Int] + d.asInstanceOf[Int]).toString
    case (1, b, c, d, e) if b == 0 && c == 0 && d == 0 && e == 0 => "1"
    case other => other.toString
  }

  def typePattern(any: Any): String = any match {
    case _: String => "String"
    case _: Int => "Integer"
    case _: Double => "Double"
    case _: Float => "Float"
    case _: List[_] => "List"
    case _: Map[_, _] => "Map"
    case _ => "other"
  }

  def typePatternWithGuard(any: Any): String = any match {
    case s: String if s.length == 5 => "String of 5 characters"
    case i: Int if i < 0 => "Integer less than zero"
    case d: Double if d > 9d => "Double greater than 9.0d"
    case f: Float if f < 3f => "Float less than 3f"
    case l: List[_] if l.size > 3 => "List holds more than 3 elements"
    case m: Map[_, _] if m.nonEmpty => "Map is not empty"
    case _ => "other"
  }

  @Test
  def testWildcardPattern(): Unit = {
    wildcardPattern(BinOp("+", Num(1D), Num(1D)))
    wildcardPattern(BinOp("+", Str("a"), Str("b")))
    wildcardPattern(UnOp("+", Num(3D)))
    wildcardPattern(Num(0D))
  }

  @Test
  def testConstantPattern(): Unit = {
    assert(constantPattern(5) == "five")
    assert(constantPattern(Nil) == "empty list")
    assert(constantPattern(Array(0)) == "something else")
    println(constantPattern(Pi))
    println(constantPattern(E))
  }

  @Test
  def testVariablePattern(): Unit = {
    assert(variablePattern(99) == "non-zero: 99")
  }

  @Test
  def testConstructorPattern(): Unit = {
    assert(constructorPattern(BinOp("+", Str("a"), Str("b"))) == "a")
    assert(constructorPattern(Str("anything else")) == "Str(anything else)")
    assert(constructorPatternWithGuard(BinOp("+", Str("a"), Str("a"))) == "Str(aa)")
    assert(constructorPatternWithGuard(BinOp("+", Num(2d), Num(2d))) == "Num(4.0)")
    assert(constructorPatternWithGuard(BinOp("*", Num(0d), Num(2d))) == "Num(0.0)")
    assert(constructorPatternWithGuard(BinOp("*", Num(2d), Num(0d))) == "Num(0.0)")
  }

  @Test
  def testSequencePattern(): Unit = {
    println(sequencePattern(List(0, 1, 2, 3)))
    println(sequencePatternWithGuard(List(0, 1, 2, 3)))
    println(sequencePattern(List(9, 1, 2, 3)))
  }

  @Test
  def testTuplePattern(): Unit = {
    assert(tuplePattern(1, "a", 3) == "Tuple3 :(1,a,3)")
    assert(tuplePattern(1, 2, 3, 4) == "Tuple4 :(1,2,3,4)")
    assert(tuplePattern(1, 2, 3, 4, "e") == "Tuple5 :(1,2,3,4,e)")
    assert(tuplePattern(1, 2, 3, 4, "e", 6) == "(1,2,3,4,e,6)")
    assert(tuplePatternWithGuard("b", "a", "c") == "bac")
    assert(tuplePatternWithGuard(1, 2, 3, 4) == "10")
    assert(tuplePatternWithGuard(1, 0, 0, 0, 0) == "1")
    assert(tuplePatternWithGuard(1, 2, 3, 4, "e") == "(1,2,3,4,e)")
    assert(tuplePatternWithGuard(1, 2, 3, 4, "e", 6) == "(1,2,3,4,e,6)")
  }

  @Test
  def testTypePattern(): Unit = {
    assert(typePattern("test") == "String")
    assert(typePattern(1) == "Integer")
    assert(typePattern(1f) == "Float")
    assert(typePattern(1d) == "Double")
    assert(typePattern(List(1, 2, 3, 4)) == "List")
    assert(typePattern(Map(1 -> "I", 2 -> "II", 10 -> "X")) == "Map")
    assert(typePatternWithGuard("tests") == "String of 5 characters")
    assert(typePatternWithGuard(-1) == "Integer less than zero")
    assert(typePatternWithGuard(1f) == "Float less than 3f")
    assert(typePatternWithGuard(10d) == "Double greater than 9.0d")
    assert(typePatternWithGuard(List(1, 2, 3, 4)) == "List holds more than 3 elements")
    assert(typePatternWithGuard(Map(1 -> "I", 2 -> "II", 10 -> "X")) == "Map is not empty")
  }

  @Test
  def testPatternOverlap(): Unit = {
    val number = Num(1.414D)
    assert(number == simplify(UnOp("-", UnOp("-", number))))
    assert(number == simplify(BinOp("+", number, Num(0D))))
    assert(Str("any") == simplify(BinOp("+", Str("any"), Str(""))))
  }

  def show(option: Option[String]) = option match {
    case Some(s) => s
    case None => "?"
  }

  @Test
  def testOptionType(): Unit = {
    val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
    println(capitals.get("France"))
    println(capitals.get("America"))
    assert("Tokyo" == show(capitals.get("Japan")))
    assert("?" == show(capitals.get("China")))
  }

  @Test
  def testDefineMultipleVariables(): Unit = {
    val myTuple = (123, "abc")
    val (number, string) = myTuple
    assert(123 == number)
    assert("abc" == string)

    val (a, b, c, d) = ("a", 'b', 123, 456f)
    assert("a" == a)
    assert('b' == b)
    assert(123 == c)
    assert(456f == d)

    // compilation error
    //val (x, y, z) = (1, 2)

    val binOp = new BinOp("+", Num(1d), Num(2d))
    val BinOp(op, Num(left), Num(right)) = binOp
    assert("+" == op)
    assert(1d == left)
    assert(2d == right)
  }

  @Test
  def partialFunction(): Unit = {
    val second: PartialFunction[List[Int], Int] = {
      case x :: y :: _ => y
    }
    // scala.MatchError: List(1) (of class scala.collection.immutable.$colon$colon)
    // second(List(1))
    assert(second.isDefinedAt(List(1, 2, 3)))
    assert(!second.isDefinedAt(List(1)))
    assert(!second.isDefinedAt(Nil))
  }

  @Test
  def patternInForExpression(): Unit = {
    val capitals = Map("Australia" -> "Sydney", "Japan" -> "Tokyo")
    for ((country, city) <- capitals) println("The capital of " + country + " is " + city)
    val results = List(Some("apple"), None, Some("orange"))
    for (Some(fruit) <- results) println(fruit)
  }
}
