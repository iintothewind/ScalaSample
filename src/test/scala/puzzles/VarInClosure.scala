package puzzles

import org.junit.Test

import collection.mutable.Buffer

class VarInClosure {
  /**
    * Closing over a free variable is not taking a “snapshot” of the variable’s value when it is used.
    * Instead, a field referencing the captured variable is added to the function object.
    * Crucially for this case, while captured vals are simply represented by the value,
    * capturing a var results in a reference to the var itself.
    *
    * From here, the explanation for the observed behavior is straightforward:
    * when each accessors1 function is created, it captures the current value of i,
    * and so prints the expected results when invoked.
    * The accessors2 functions, on the other hand, each capture a reference to a mutable IntRef object containing the value of j,
    * which can change over time.
    * By the time the first accessors2 function is invoked, the value of j is already 3.
    * Since data only has three elements, invoking data(j) triggers an IndexOutOfBoundsException
    */
  @Test(expected = classOf[IndexOutOfBoundsException])
  def runPuzzle(): Unit = {
    val accessors1 = Buffer.empty[() => Int]
    val accessors2 = Buffer.empty[() => Int]
    val data = Seq(100, 110, 120)
    var j = 0
    for (i <- 0 until data.length) {
      accessors1 += (() => data(i))
      accessors2 += (() => data(j))
      j += 1
    }
    accessors1.foreach(a1 => println(a1()))
    accessors2.foreach(a2 => println(a2()))
  }

  /**
    * The most robust way to prevent this problem is to avoid vars, which is also better Scala style.
    * If you can’t avoid a var, but you still want a closure to capture its value at the time the closure is created,
    * you can “freeze” the var by assigning its value to a temporary val.
    *
    * Avoid capturing free variables in your closures that refer to anything mutable—vars or mutable objects.
    * If you need to close over anything mutable, extract a stable value and assign it to a val, then use that val in your function
    */
  @Test
  def freezeVar(): Unit = {
    val accessors2 = Buffer.empty[() => Int]
    val data = Seq(100, 110, 120)
    var j = 0
    for (i <- 0 until data.length) {
      val currentJ = j
      accessors2 += (() => data(currentJ))
      j += 1
    }
    accessors2.foreach(a2 => println(a2()))
  }

}
