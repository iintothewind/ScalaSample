package scala.structure

import org.junit.Test

class IfSample {
  @Test
  def valReturn(): Unit = {
    val test = ""
    assert("empty" == (if (test.isEmpty) "empty" else "non-empty"))
  }
}
