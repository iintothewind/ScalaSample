package scala.abstracts

import java.io.File

import org.junit.Test

import scala.io.Source
import scala.language.reflectiveCalls

class QuickTour {

  trait Abstract {
    // define an abstract type means to give a alias for a type
    // why use an abstract type? 1. use as a short alias instead of a verbose one 2. declared types must be defined in subclasses
    type T

    // define an abstract method means to give method name, parameters, and return type but no implementation
    def transform(x: T): T

    // define an abstract val is to give its name and type but not its value
    val initial: T
    var current: T
  }

  class Concrete extends Abstract {
    // give the real type to the alias in concrete class
    override type T = String

    override def transform(x: T): T = x.toUpperCase

    override val initial: T = "Hi"
    override var current: T = initial
  }

  @Test
  def testConcrete(): Unit = {
    val concrete = new Concrete
    assert("HI" == concrete.transform(concrete.current))
  }

  abstract class Fruit {
    val v: String

    def m: String
  }

  abstract class Apple extends Fruit {
    val v: String
    val m: String // OK to override a def with a val
  }

  class ConcreteApple extends Fruit {
    override val v: String = "Apple"

    override def m: String = "Apple"
  }

  abstract class BadApple extends Fruit {
    // overriding value v in class Fruit of type String
    // method v needs to be stable, immutable value
    //def v: String // Error
    def m: String
  }

  trait AbstractTime {
    var hour: Int
    var minute: Int
  }

  // definition of AbstactTime is exactly equivalent to the definition of AbstractMethodTime
  trait AbstractMethodTime {
    def hour: Int

    def hours_=(x: Int)

    def minute: Int

    def minute_=(x: Int)
  }

  class ConcreteTime extends AbstractTime {
    private[this] var h = 0
    private[this] var m = 0

    def hour: Int = h

    def hour_=(x: Int) = h = x

    def minute: Int = m

    def minute_=(x: Int) = m = x

    override def toString: String = h + ":" + m
  }

  @Test
  def testConcreteTime(): Unit = {
    val time = new ConcreteTime
    time.hour = 12
    time.minute = 15
    assert("12:15" == time.toString)
  }

  trait RationalTrait {
    val numerArg: Int
    val denomArg: Int
    require(denomArg != 0)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private val g = gcd(numerArg, denomArg)
    val numerator = numerArg / g
    val denominator = denomArg / g

    override def toString = numerator + "/" + denominator
  }

  @Test(expected = classOf[IllegalArgumentException])
  def initRationalTraitDirectly(): Unit = {
    //runtime error because denomArg has its default value 0 when RationalTrait is initialized
    new RationalTrait {
      override val numerArg: Int = 1
      override val denomArg: Int = 3
    }
  }

  @Test
  def preInitializeInAnonymousClass(): Unit = {
    val rational = new {
      val numerArg = 2
      val denomArg = 6
    } with RationalTrait
    assert("1/3" == rational.toString)
  }

  @Test
  def preInitializeInSubclass(): Unit = {
    class Rational(private[this] val n: Int, private[this] val d: Int) extends {
      val numerArg = n
      val denomArg = d
    } with RationalTrait {
      def +(that: Rational): Rational = new Rational(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)

      override def toString: String = super.toString
    }
    assert("1/3" == new Rational(2, 6).toString)
    assert("1/2" == (new Rational(1, 6) + new Rational(2, 6)).toString)
  }

  @Test
  def preInitializeInObject(): Unit = {
    object rational extends {
      val numerArg = 2
      val denomArg = 6
    } with RationalTrait
    assert("1/3" == rational.toString)
  }

  object Demo {
    lazy val x = {
      println("initializing x")
      "done"
    }
  }

  @Test
  def objectDemoLazyInit(): Unit = {
    println(Demo)
    // x will be initialized when the first time it is used
    println(Demo.x)
    // done is stored after x has been initialized
    println(Demo.x)
  }

  trait LazyRationalTrait {
    val numerArg: Int
    val denomArg: Int

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private lazy val g = {
      require(denomArg != 0)
      gcd(numerArg, denomArg)
    }
    lazy val numerator = numerArg / g
    lazy val denominator = denomArg / g

    override def toString = numerator + "/" + denominator
  }

  @Test
  def lazyInitRational(): Unit = {
    val lazyRational = new LazyRationalTrait {
      override val numerArg: Int = 1
      override val denomArg: Int = 3
    }
    // numerator and denominator would be initialized when to lazyRational.toString is called
    assert("1/3" == lazyRational.toString)
  }

  abstract class Food

  abstract class Pet {
    def eat(food: Food)
  }

  abstract class Animal {
    // Path dependent type, means the type depends on path, different path give rise to different types
    // SuitableFood must be a subtype of Food
    type SuitableFood <: Food

    def eat(food: SuitableFood)
  }

  class Grass extends Food

  class Meat extends Food

  class Cat extends Pet {
    override def eat(food: Food): Unit = {
      println(this.getClass.getSimpleName.concat("s eat ").concat(food.getClass.getSimpleName))
    }
  }

  class Cow extends Animal {
    override type SuitableFood = Grass

    override def eat(food: SuitableFood): Unit = {
      println(this.getClass.getSimpleName.concat("s eat ").concat(food.getClass.getSimpleName))
    }
  }

  @Test
  def pathDependentType(): Unit = {
    val cat = new Cat
    // Cats would eat grass if no type specified for parameter food
    cat.eat(new Grass)

    // Cow's super class Animal, is using a path dependent type: SuitableFood
    val cow = new Cow
    //compilation error, the expected type should be SuitableFood, in this case it should be Grass, not Meat
    //cow.eat(new Meat)
    cow.eat(new Grass)
  }

  /**
   * loading uses &lt;: to define that T must be a subtype of the base type {def close():Unit}
   * therefore, closeable.close() can be used in finally block
   */
  def loading[T <: {def close() : Unit}, S](closeable: T)(operation: T => S): S = {
    try {
      operation(closeable)
    } finally {
      // the call to close() needs enable (import) scala.language.reflectiveCalls
      closeable.close()
    }
  }

  @Test
  def structuralType(): Unit = {
    //BufferedReader and other actual parameters have the same father type: {def close() : Unit}
    assert(loading(Source.fromFile(new File("build.sbt")).bufferedReader())(_.readLine()).contains("Scala"))
  }

  object Color extends Enumeration {
    // Value will be assigned from left to right
    val Red, Green, Blue = Value
  }

  object Direction extends Enumeration {
    val North = Value("North")
    val East = Value("East")
    val South = Value("South")
    val West = Value("West")
  }

  @Test
  def enumeration(): Unit = {
    assert(Color.Blue > Color.Green)
    Color.values.foreach(v => println("name: " + v.getClass.getCanonicalName + ", id: " + v.id))
    Direction.values.foreach(println)
  }

}
