package puzzles

import org.junit.Test
import puzzles.Location.BMember
import puzzles.Location.BConstructor
import puzzles.Location.BEvery

object Location {

  trait A {
    val audience: String
    println("Hello " + audience)
  }

  class BMember(a: String = "World") extends A {
    val audience = a
    println("I repeat: Hello " + audience)
  }

  class BConstructor(override val audience: String = "World") extends A {
    println("I repeat: Hello " + audience)
  }


  trait B {
    val audience: String
    println("Hello " + audience)
  }

  trait AfterB {
    val introduction: String
    println(introduction)
  }

  class BEvery(override val audience: String) extends {
    override val introduction = {
      println("Evaluating early def")
      "Are you there?"
    }
  } with B with AfterB {
    println("I repeat: Hello " + audience)
  }


}


class Location {
  /** According to the language specification, the initialization sequence for
    * new BMember("Readers") and new BConstructor("Readers") will be:
    *1. The argument "Readers" is evaluated. In this case, there is nothing to do here,
    * but if the argument were specified as an expression (e.g., "readers".capitalize), this would be evaluated first.
    *2. The class being constructed is initialized by evaluating the template:
    * superclass { statements }
    * a) First, the superclass constructor A
    * b) Then the statement sequence in the body of the subclass, either BMember or BConstructor
    * Here, note that we are omitting details regarding traits, etc., that do not apply to this example.
    * In the case of BMember, "Readers" is assigned to the constructor parameter, a, in the first step.
    * When A’s constructor is invoked, audience is still uninitialized, so the default string value null is printed.
    * "Readers" is assigned to audience, and then printed, only when the state- ment sequence in the body of BMember executes.
    * BConstructor’s case is different: here "Readers" is evaluated and as- signed to audience straight away,
    * as part of the evaluation of the construc- tor arguments.
    * The value of audience is already "Readers" by the time A’s constructor is invoked.
    *
    * Answer:
    * Hello null
    * I repeat: Hello Readers
    * Hello Readers
    * I repeat: Hello Readers
    */
  @Test
  def runPuzzleA(): Unit = {
    new BMember("Readers")
    new BConstructor("Readers")
  }

  /**
    * Answer:
    * Evaluating param
    * Evaluating early def
    * Hello Readers
    * Are you there?
    * I repeat: Hello Readers
    */
  @Test
  def runPuzzleB(): Unit = {
    new BEvery({
      println("Evaluating param")
      "Readers"
    })
  }

}
