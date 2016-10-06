package puzzles

import org.junit.Test

class MapComprehension {
  val xs = Seq(Seq("a", "b", "c"), Seq("d", "e", "f"),
    Seq("g", "h"), Seq("i", "j", "k"))

  /**
    * In Scala, the i <- 0 to 1 syntax is called a generator.
    * With generators that perform a simple variable assignment, it’s easy to forget that the generator’s left-hand side is not a simple variable,
    * it’s a pattern, as demonstrated by Seq(x, y, z) <- xs in the code sample.
    * The Scala compiler desugars generators with non-trivial patterns (i.e., patterns that constistute more than a simple variable assignment) differently.
    * This expression:
    * for (pattern <- expr) yield fun
    * ends up being rewritten as:
    * expr map { case pattern => fun }
    *
    * So far, this is identical to our own desugaring in the example.
    * But what the language specification also stipulates for a non-trivial pattern is the addition of a withFilter invocation.
    * Thus the following expression:
    * for (pattern <- expr) yield fun
    * actually becomes:
    *
    * expr withFilter {
    * case pattern => true
    * case _ => false
    * } map { case pattern => fun }
    *
    * It is this withFilter invocation that transparently “strips out” the nonmatching value that causes the MatchError in our attempted desugaring.
    */
  @Test
  def forComprehension(): Unit = {
    val ys = for (Seq(x, y, z) <- xs) yield x + y + z
    ys.foreach(println)
  }

  @Test
  def forComprehensionSimilarity(): Unit = {
    val ys = xs.withFilter{
      case Seq(_,_,_) => true
      case _ => false
    }.map{
      case Seq(x,y,z) => x+y+z
    }
    ys.foreach(println)
  }

  @Test(expected = classOf[MatchError])
  def mapCase(): Unit = {
    val zs = xs map { case Seq(x, y, z) => x + y + z } // scala.MatchError: List(g, h)
  }


}
