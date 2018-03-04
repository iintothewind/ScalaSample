package fp.intro

import org.junit.Test

class TreeSample {
  //Some(Branch(Some(Leaf("D"))), "B", Some(Leaf("E"))))
  @Test
  def test_dforeach(): Unit = {
    //Branch(Some(Branch(Some(Leaf("D")), "B", Some(Leaf("E")))), "A", Some(Branch(Some(Leaf("F")), "C", Some(Leaf("G"))))).dforeach(println)
    Branch(Some(Branch(Some(Leaf("D")), "B", Some(Leaf("E")))), "A", Some(Branch(Some(Leaf("F")), "C", Some(Leaf("G"))))).bforeach(println)
  }
}