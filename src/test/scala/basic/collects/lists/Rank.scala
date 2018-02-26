package basic.collects.lists

import java.util.concurrent.TimeUnit

import com.google.common.base.Stopwatch
import org.junit.Test
import util.Random

class Rank {
  val list: List[Int] = List.fill(50000)(Random.nextInt(2000))

  def cost(cnt: Int)(call: => Unit): Long = {
    val watch = Stopwatch.createStarted()
    1.to(cnt).foreach(_ => call)
    watch.stop()
    watch.elapsed(TimeUnit.MILLISECONDS)
  }

  @Test
  def scanLeftSolution(): Unit = {
    val reverseSorted = list.sortBy(-_)
    //println(reverseSorted)
    val ranked = reverseSorted.tail.scanLeft((1, reverseSorted.head)) { case ((i, x), y) => (if (x == y) i else i + 1, y) }
    //println(ranked)
  }

  @Test
  def foldLeftSolution(): Unit = {
    val reverseSorted = list.sortBy(-_)
    val ranked = reverseSorted.foldLeft(List((0, 0))) {
      case (rank, value) if rank.head._2 == value => (rank.head._1, value) :: rank
      case (rank, value) if rank.head._2 != value => (rank.head._1 + 1, value) :: rank
    }.reverse.tail
    //println(ranked)
  }

  @Test
  def testPerf(): Unit = {
    println(cost(200)(scanLeftSolution()))
    println(cost(200)(foldLeftSolution()))
  }


}
