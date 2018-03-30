package war

import org.junit.Test

class Scramble {
  def scramble(a: String, b: String): Boolean = {
    val ma = Option(a).map(s => s.foldLeft(Map.empty[Char, Int])((m, c) => m.+(c -> (m.getOrElse(c, 0) + 1)))).getOrElse(Map.empty)
    val mb = Option(b).map(s => s.foldLeft(Map.empty[Char, Int])((m, c) => m.+(c -> (m.getOrElse(c, 0) + 1)))).getOrElse(Map.empty)
    (ma, mb) match {
      case (x, y) if y.forall(kv => kv._2 <= ma.getOrElse(kv._1, 0)) => true
      case _ => false
    }
  }


  @Test
  def testScramble(): Unit = {
    assert(scramble("rkqodlw", "world"))
    assert(scramble("cedewaraaossoqqyt", "codewars"))
    assert(!scramble("katas", "steak"))
    assert(!scramble("scriptjavx", "javascript"))
    assert(scramble("scriptingjava", "javascript"))
    assert(scramble("scriptsjava", "javascripts"))
    assert(!scramble("javscripts", "javascript"))
    assert(scramble("aabbcamaomsccdd", "commas"))
    assert(scramble("commas", "commas"))
    assert(scramble("sammoc", "commas"))
  }
}
