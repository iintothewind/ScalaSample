package war

import fp.combinator.Gn
import fp.intro.Rng
import org.junit.Test

import scala.annotation.tailrec

class ScsSolution {
  def overlap(l: String, r: String): String = {
    @tailrec
    def loop(l: String, r: String, restOfRLen: Int): String = (l, r) match {
      case (a, b) if a.endsWith(b.dropRight(restOfRLen)) => a.dropRight(b.length - restOfRLen).concat(b)
      case pair@_ => loop(pair._1, pair._2, restOfRLen + 1)
    }

    (l, r) match {
      case (a, b) if a.contains(b) => a
      case (a, b) if b.contains(a) => b
      case it@_ => List(loop(it._1, it._2, 0), loop(it._2, it._1, 0)).minBy(_.length)
    }
  }

  def brutalScs(seq: Seq[String]): String = {
    Option(seq).getOrElse(Seq.empty)
      .distinct
      .filter(a => seq.filterNot(a == _).forall(!_.contains(a)))
      .permutations
      .toList
      .map(sp => sp.foldLeft("")(overlap))
      .minBy(_.length)
  }

  @tailrec
  final def greedyScs(seq: Seq[String]): String = {
    Option(seq) match {
      case xsp@Some(_ +: _ +: _) => xsp.get.combinations(2)
        .map { case l +: r +: _ => (l, r, overlap(l, r)) }
        .maxBy(t => t._1.length + t._2.length - t._3.length) match {
        case (a, b, overlapped) if (a.length + b.length - overlapped.length) > 0 =>
          greedyScs(overlapped +: xsp.get.filterNot(x => x == a || x == b))
        case _ => xsp.get.mkString("")
      }
      case Some(x +: Nil) => x
      case _ => ""
    }
  }

  @Test
  def testOverlap(): Unit = {
    println(overlap("ABCD", "BCDA"))
  }


  @Test
  def testGenSeq(): Unit = {
    val seq = Gn.randStr("TCGA", Gn.const(10))(Gn.between(1, 5)).sample(Rng(2))
    println(seq.mkString(","))
    println("brutalScs:")
    println(brutalScs(seq))
  }

  @Test
  def testGreedyScs(): Unit = {
    //    val seq = Seq("ABC", "BCA", "CAB")
    val seq = Gn.randStr("TCGA", Gn.const(10))(Gn.between(1, 5)).sample(Rng(2))
    println(seq.mkString(","))
    println("brutalScs:")
    println(brutalScs(seq))
    println("greedyScs:")
    println(greedyScs(seq))
  }
}
