package scala.pattern

import org.junit.Test

class CaseExample {

  @Test
  def caseVal(): Unit = {
    val v = Str("x")
    println(v)
    assert("x" == v.name)
  }

  @Test
  def caseNumber(): Unit = {
    val number = Num(1.414D)
    println(number)
    assert(1.414D == number.num)
  }

  @Test
  def caseUnOp(): Unit = {
    val unOp = UnOp("+", Str("unary"))
    println(unOp)
    assert(Str("unary") == unOp.arg)
  }

  @Test
  def caseBinOp(): Unit = {
    val binOp = BinOp("+", Num(1D), Num(2D))
    println(binOp)
    assert(Num(1d) == binOp.left)
  }


  case class Person(name: String, age: Int)

  //You only include the @ when you want to also deal with the object itself
  //Otherwise, there's no real point in including it.
  def ageMatcher(person: Person): Unit = {
    person match {
      case p@Person(_, age) if p.name == "Bill" => println(s"Bill's age = $age")
      case Person(_, age) => println(s"person age = $age")
      case _ => println("Not a person")
    }
  }

  @Test
  def atOperator(): Unit = {
    val intValue = Some(2)
    intValue match {
      case v@Some(_) if v.get < 0 => println(s"v == $v")
      case _ => println("somthing else")
    }

    val bill = Person("Bill", 55)
    val lara = Person("Lara", 22)
    val mike = Person("Mike", 23)
    this.ageMatcher(bill)
    this.ageMatcher(lara)
    this.ageMatcher(mike)
  }

  @Test
  def copy(): Unit = {
    val bill = Person("Bill", 55)
    // use caseClass.copy() to create a copy and change the properties
    this.ageMatcher(bill.copy(name = "Sam", age = 30))
  }

  type Peg = String
  type Move = (String, String)

  def hanoi(n: Int, left: Peg, right: Peg, temp: Peg): List[Move] = n match {
    case i if i < 1 => List.empty
    case i if i == 1 => List((left, right))
    case i if i > 1 => hanoi(i - 1, left, temp, right) ++ hanoi(1, left, right, temp) ++ hanoi(i - 1, temp, right, left)
  }

  @Test
  def testHanoi(): Unit = {
    assert(List.empty[Move] == hanoi(-1, "a", "b", "c"))
    assert(List.empty[Move] == hanoi(0, "a", "b", "c"))
    assert(List(("a", "b")) == hanoi(1, "a", "b", "c"))
    assert(List(("a", "c"), ("a", "b"), ("c", "b")) == hanoi(2, "a", "b", "c"))
    assert(List(("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")) == hanoi(3, "a", "b", "c"))
  }
}
