package puzzles

import org.junit.Test
import puzzles.Initialization.C1
import puzzles.Initialization.C2
import puzzles.Initialization.C3
import puzzles.Initialization.C4

/**
  * Default initial values
  * For the record, Scala specifies default initial values as:
  * • 0 for Byte, Short, and Int
  * • 0L, 0.0f, and 0.0d for Int, Long, Float, and Double, respectively
  * • '\0' for Char
  * • false for Boolean
  * • () for Unit
  * • null for all other types
  */
object Initialization {

  trait A1 {
    val foo: Int
    val bar = 10
    println("In A: foo: " + foo + ", bar: " + bar)
  }

  class B1 extends A1 {
    val foo: Int = 25
    println("In B: foo: " + foo + ", bar: " + bar)
  }

  class C1 extends B1 {
    override val bar = 99
    println("In C: foo: " + foo + ", bar: " + bar)
  }

  trait A2 {
    val foo: Int

    def bar: Int = 10

    println("In A: foo: " + foo + ", bar: " + bar)
  }

  class B2 extends A2 {
    val foo: Int = 25
    println("In B: foo: " + foo + ", bar: " + bar)
  }

  class C2 extends B2 {
    override def bar: Int = 99

    println("In C: foo: " + foo + ", bar: " + bar)
  }

  trait A3 {
    val foo: Int
    lazy val bar = 10
    println("In A: foo: " + foo + ", bar: " + bar)
  }

  class B3 extends A3 {
    val foo: Int = 25
    println("In B: foo: " + foo + ", bar: " + bar)
  }

  class C3 extends B3 {
    override lazy val bar = 99
    println("In C: foo: " + foo + ", bar: " + bar)
  }

  trait A4 {
    val foo: Int
    val bar = 10
    println("In A: foo: " + foo + ", bar: " + bar)
  }

  class B4 extends A4 {
    val foo: Int = 25
    println("In B: foo: " + foo + ", bar: " + bar)
  }

  class C4 extends {
    override val bar = 99
  } with B4 {
    println("In C: foo: " + foo + ", bar: " + bar)
  }

}

/**
  * First, you should remember that every Scala class has a primary constructor that is not explicitly defined,
  * but interwoven with the class definition.
  * All statements in the class definition form the body of the primary
  * constructor, and that includes field definitions
  * (which is, by the way, the reason Scala does not intrinsically differentiate between class fields and values local to the constructor).
  * Hence, all the code in trait A and classes B and C belongs to the constructor body.
  * The following rules control the initialization and overriding behavior of vals:
  * 1. Superclasses are fully initialized before subclasses.
  * 2. Members are initialized in the order they are declared.
  * 3. When a val is overridden, it can still only be initialized once.
  * 4. Like an abstract val, an overridden val will have a default initial value
  * during the construction of superclasses.
  */
class Initialization {

  /**
    * Therefore, even though bar appears to have an initial value assigned in
    * trait A and class B, that is not the case, because it is overridden in class C.
    * This means that during the construction of trait A, bar has the default initial value of 0 and not the assigned value of 10.
    * Essentially, initialization order gets in the way, and the assignment of 10 to bar in trait A is completely invisible
    * because bar is overridden in class C, where it is initialized to 99.
    * Similarly, the value foo, since it is assigned a non-default value in class B, has value 0 in A and then 25 in B and C
    * In A: foo: 0, bar: 0
    * In B: foo: 25, bar: 0
    * In C: foo: 25, bar: 99
    */
  @Test
  def runPuzzle(): Unit = {
    new C1()
  }

  /**
    * The reason defining bar as a def works here is that method bodies do not belong to the primary constructor and,
    * therefore, take no part in class initialization.
    * In A: foo: 0, bar: 99
    * In B: foo: 25, bar: 99
    * In C: foo: 25, bar: 99
    */
  @Test
  def overrideDef(): Unit = {
    new C2()
  }

  /**
    * Declaring bar as a lazy val means it will be initialized to 99 during the construction of trait A,
    * since that is where it is accessed for the first time.
    * Lazy vals are initialized using compiler-generated methods, and here,
    * the overridden version in trait C is the one that is called.
    * In A: foo: 0, bar: 99
    * In B: foo: 25, bar: 99
    * In C: foo: 25, bar: 99
    *
    * using lazy vals can have some disadvantages:
    * 1. They incur a slight performance cost, due to synchronization that happens under the hood.
    * 2. You cannot declare an abstract lazy val.
    * 3. Using lazy vals is prone to creating cyclic references that can result in stack overflow errors on first access, or possibly even deadlock.
    * 4. You can even get a deadlock when a cyclic dependency does not exist between lazy vals, but between objects that declare them. Such
    * scenarios can be very subtle and non-obvious.
    */
  @Test
  def useLazyVal(): Unit = {
    new C3()
  }

  /**
    * In A: foo: 0, bar: 99
    * In B: foo: 25, bar: 99
    * In C: foo: 25, bar: 99
    */
  @Test
  def usePreInitializedField(): Unit = {
    new C4()
  }

}
