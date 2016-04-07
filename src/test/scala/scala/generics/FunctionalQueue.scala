package scala.generics

import org.junit.Test

import scala.collection.immutable.Queue

class FunctionalQueue {

  @Test
  def queue(): Unit = {
    // Queue supports 3 operations: head, tail, enqueue
    val q = Queue(1, 2, 3)
    // Queue is similar as List, the difference is that
    // List is usually extended at the the front, using :: operation
    // Queue is extended at the end, using enqueue
    val enq = q.enqueue(4)
    assert(q != enq)
  }

  @Test
  def functionalQueue(): Unit = {
    val tq = new LisQueue(Nil, List(1, 2, 3))
    assert(3 == tq.head)
    assert(Queue(3, 2, 1, 4) == tq.enqueue(4).toQueue)
    assert(Queue(2, 1) == tq.tail.toQueue)
    val hq = new LisQueue(List(1, 2, 3), Nil)
    assert(1 == hq.head)
    assert(Queue(1, 2, 3, 4) == hq.enqueue(4).toQueue)
    assert(Queue(2, 3) == hq.tail.toQueue)
    val bq = new LisQueue(List(1, 2, 3), List(4, 5, 6))
    assert(1 == bq.head)
    // enqueue(x) uses :: operation to append element x, so x will be added at the beginning of the second list
    assert(Queue(1, 2, 3, 6, 5, 4, 7) == bq.enqueue(7).toQueue)
    assert(Queue(2, 3, 6, 5, 4) == bq.tail.toQueue)
  }


  @Test
  def privateMainConstructor(): Unit = {
    val lq = LisQueue.empty[Int]()
    assert(Queue(4) == lq.enqueue(4).toQueue)
    val hq = HidQueue(1, 2, 3)
    assert(1 == hq.head)
    assert(Queue(1, 2, 3, 4) == hq.enqueue(4).toQueue)
    assert(Queue(2, 3) == hq.tail.toQueue)
  }

  @Test
  def hideImplInObject(): Unit = {
    val absQueue = AbsQueue(1, 2, 3)
    assert(1 == absQueue.head)
    assert(Queue(1, 2, 3, 4) == absQueue.enqueue(4).toQueue)
    assert(Queue(2, 3) == absQueue.tail.toQueue)
  }

  trait Fruit {
    override def toString: String = this.getClass.getSimpleName
  }

  class Apple extends Fruit {
    override def toString: String = super.toString
  }

  class Lemon extends Fruit {
    override def toString: String = super.toString
  }

  class Peach extends Fruit {
    override def toString: String = super.toString
  }

  @Test
  def covariant(): Unit = {
    val apple = new Apple
    val lemon = new Lemon
    val peach = new Peach
    val lisQueue = LisQueue[Apple](apple)
    // wont compile, LisQueue does not support covariant
    // println(lisQueue.enqueue(lemon).toQueue)
    val subQueue = SubQueue[Apple](apple)
    assert(subQueue.isInstanceOf[SubQueue[Apple]])
    // SubQueue is covariant, so it can accept Peach which is a subclass of Fruit
    // and return a new SubQueue[Fruit]
    assert(subQueue.enqueue(peach).isInstanceOf[SubQueue[Fruit]])
  }

  class Publication(val title: String)

  class Book(title: String) extends Publication(title)

  object Library {
    val books: Set[Book] = Set(new Book("Programmng in Scla"), new Book("Walden"))

    def printBookList(info: Book => AnyRef): Unit = {
      for (book <- books) println(info(book))
    }
  }

  @Test
  def contravariant(): Unit = {
    // the type of actual parameter for p is Book, which extends from Publication
    def getTitle(p: Publication): String = p.title
    Library.printBookList(getTitle)
  }

  /**
   * Scala's variance checking rules contain a special case for object private definitions.
   * Such definitions are omitted when it is checked that a type parameter
   * with either a + or - annotation occurs only in positions that have the same variance classification.
   * Since leading and trailing can only be access within class SmartQueue, +T will never affect them.
   */
  @Test
  def objectPrivateVar(): Unit = {
    val apple = new Apple
    val lemon = new Lemon
    val peach = new Peach
    val smartQueue = SmartQueue[Apple](apple)
    assert(smartQueue.isInstanceOf[SmartQueue[Apple]])
    assert(smartQueue.enqueue(peach).isInstanceOf[SmartQueue[Fruit]])
  }


  @Test
  def testMergeSort(): Unit = {
    val adaWong = Person("Ada", "Wong")
    val leonShen = Person("Leon", "Bratheon")
    val michaelAvalon = Person("Michael", "Avalon")
    val list = List(adaWong, leonShen, michaelAvalon)
    println(mergeSort(list))
  }


}
