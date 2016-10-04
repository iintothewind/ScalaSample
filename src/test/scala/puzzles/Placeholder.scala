package puzzles

import org.junit.Test

/**
  * Scala places a strong emphasis on writing simple, concise code.
  * Its syntax for anonymous functions, arg => expr, makes it easy to construct function literals with minimal boilerplate, even when the functions consist of multiple statements.
  * For functions with self-explanatory parameters, you can do better and use placeholder syntax.
  * This trims away the parameter declaration. For example:
  * List(1, 2).map { i => i + 1 }
  * becomes:
  * List(1, 2).map { _ + 1 }
  * The following two statements are equivalent:
  * scala> List(1, 2).map { i => i + 1 }
  * res1: List[Int] = List(2, 3)
  * scala> List(1, 2).map { _ + 1 }
  * res0: List[Int] = List(2, 3)
  * What if you added a debugging statement to the above example to help you understand when the function is applied?
  * What is the result of executing the following code in the REPL?
  * List(1, 2).map { i => println("Hi"); i + 1 }
  * List(1, 2).map { println("Hi"); _ + 1 }
  */
class Placeholder {
  /**
    * { i => println("Hi"); i + 1 }, is identified as one function literal expression of the form arg => expr,
    * with expr here being the block, println("Hi"); i + 1.
    * Since the println statement is part of the function body,
    * it is executed each time the function is invoked.
    */
  @Test
  def testAnonymouseFunction(): Unit = {
    List(1, 2).map { i => println("Hi"); i + 1 }
  }

  /**
    * the code block is identified as two expressions: println("Hi") and _ + 1.
    * The block is executed, and the last expression
    * (which is conveniently of the required function type, Int => Int) is passed to map.
    * The println statement is not part of the function body.
    * It is invoked when the argument to map is evaluated, not as part of the execution of map.
    */
  @Test
  def testPlaceholder(): Unit = {
    List(1, 2).map { println("Hi"); _ + 1 }
  }


}
