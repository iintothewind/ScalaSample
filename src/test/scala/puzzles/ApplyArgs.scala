package puzzles

import org.junit.Test
import puzzles.ApplyArgs.nextInt
import puzzles.ApplyArgs.nextNumber
import puzzles.ApplyArgs.applyNMulti
import puzzles.ApplyArgs.applyNCurried

object ApplyArgs {
  def applyNMulti[T](n: Int)(arg: T, f: T => T) =
    (1 to n).foldLeft(arg) { (acc, _) => f(acc) }

  def applyNCurried[T](n: Int)(arg: T)(f: T => T) =
    (1 to n).foldLeft(arg) { (acc, _) => f(acc) }

  def nextInt(n: Int) = n * n + 1

  def nextNumber[N](n: N)(implicit numericOps: Numeric[N]) =
    numericOps.plus(numericOps.times(n, n), numericOps.one)

}

class ApplyArgs {
  /**
    * Isn’t N bound to Double by the use of 2.0 as the first argument, though?
    * Yes, it is, but the key point here is that this does not help the compiler.
    * The compiler attempts to satisfy the type requirements for each parameter in a parameter list individually,
    * and is therefore not able to use information about a generic type provided by other arguments in the same parameter list.
    * In this case, the fact that the generic type N is bound to a specific type, Double,
    * is not available when the compiler searches for the appropriate numericOps.
    * This is true even though the parameter that binds N appears before nextNumber in the parameter list!
    * In the curried case, on the other hand, N has been bound to Double as part of the evaluation of the previous parameter list,
    * as opposed to an earlier parameter in the same list.
    */
  @Test
  def runPuzzle(): Unit = {
    println(applyNMulti(3)(2, nextInt)) // 677
    println(applyNCurried(3)(2)(nextInt)) //677
    // compilation error: Error:(28, 33) could not find implicit value for parameter numericOps: Numeric[N]
    // println(applyNMulti(3)(2.0, nextNumber))
    // Compilation error: Type mismatch, expected (Double)=>Double, actual (Nothing)=>Nothing
    // println(applyNCurried(3)(2.0)(nextNumber))
  }

  @Test
  def fixByExplicitlySpecifyingType(): Unit = {
    assert(applyNMulti(3)(2.0, nextNumber[Double]) == 667.0)
    assert(applyNMulti[Double](3)(2.0, nextNumber[Double]) == 667.0)
  }
}
