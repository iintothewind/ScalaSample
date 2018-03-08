package basic.objects

import scala.collection.immutable.Queue

package object generics {

  class LisQueue[T](private val leading: List[T], private val trailing: List[T]) {
    private def mirror: LisQueue[T] = if (leading.isEmpty) new LisQueue(trailing.reverse, Nil) else this

    def head: T = mirror.leading.head

    def tail: LisQueue[T] = {
      val q = mirror
      new LisQueue(q.leading.tail, q.trailing)
    }

    def enqueue(x: T): LisQueue[T] = new LisQueue(leading, x :: trailing)

    def toQueue: Queue[T] = Queue.newBuilder.++=(leading ++: trailing.reverse).result()
  }

  object LisQueue {
    def apply[T](xs: T*): LisQueue[T] = new LisQueue[T](xs.toList, Nil)

    def empty[T](): LisQueue[T] = new LisQueue(Nil, Nil)
  }

  // add private modifier in front of primary constructor to hide it from public access
  class HidQueue[T] private(private val leading: List[T], private val trailing: List[T]) {
    private def mirror: HidQueue[T] = if (leading.isEmpty) new HidQueue(trailing.reverse, Nil) else this

    def head: T = mirror.leading.head

    def tail: HidQueue[T] = {
      val q = mirror
      new HidQueue(q.leading.tail, q.trailing)
    }

    def enqueue(x: T): HidQueue[T] = new HidQueue(leading, x :: trailing)

    def toQueue: Queue[T] = Queue.newBuilder.++=(leading ++: trailing.reverse).result()
  }

  object HidQueue {
    def apply[T](xs: T*): HidQueue[T] = new HidQueue(xs.toList, Nil)

    def empty[T](): HidQueue[T] = new HidQueue(Nil, Nil)
  }

  class SubQueue[+T] private(private val leading: List[T], private val trailing: List[T]) {
    //“U >: T”, defines T as the lower bound for U
    private def mirror: SubQueue[T] = if (leading.isEmpty) new SubQueue(trailing.reverse, Nil) else this

    //U is required to be a supertype of T
    def head: T = mirror.leading.head

    //The parameter to enqueue is now of type U instead of type T,
    //and the return value of the method is now SubQueue[U] instead of SubQueue[T]
    def tail: SubQueue[T] = {
      val q = mirror
      new SubQueue(q.leading.tail, q.trailing)
    }

    def enqueue[U >: T](x: U): SubQueue[U] = new SubQueue[U](leading, x :: trailing)

    def toQueue: Queue[T] = Queue.newBuilder.++=(leading ++: trailing.reverse).result()
  }

  object SubQueue {
    def apply[T](elements: T*): SubQueue[T] = new SubQueue(elements.toList, Nil)
  }

  trait AbsQueue[T] {
    def head: T

    def tail: AbsQueue[T]

    def enqueue(x: T): AbsQueue[T]

    def toQueue: Queue[T]
  }

  object AbsQueue {
    def apply[T](elements: T*): AbsQueue[T] = new AbsQueueImpl(elements.toList, Nil)

    private class AbsQueueImpl[T](private val leading: List[T], private val trailing: List[T]) extends AbsQueue[T] {
      private def mirror: AbsQueueImpl[T] = if (leading.isEmpty) new AbsQueueImpl(trailing.reverse, Nil) else this

      override def head: T = mirror.leading.head

      override def tail: AbsQueue[T] = {
        val q = mirror
        new AbsQueueImpl(q.leading.tail, q.trailing)
      }

      override def enqueue(x: T): AbsQueue[T] = new AbsQueueImpl(leading, x :: trailing)

      def toQueue: Queue[T] = Queue.newBuilder.++=(leading ++: trailing.reverse).result()
    }

  }

  /**
   * Scala's variance checking rules contain a special case for object private definitions.
   * Such definitions are omitted when it is checked that a type parameter
   * with either a + or - annotation occurs only in positions that have the same variance classification.
   * Since leading and trailing can only be access within class SmartQueue, +T will never affect them.
   */
  class SmartQueue[+T] private(private[this] var leading: List[T], private[this] var trailing: List[T]) {
    private def mirror(): Unit = if (leading.isEmpty) {
      while (trailing.nonEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail
      }
    }

    def head: T = {
      mirror()
      leading.head
    }

    def tail: SmartQueue[T] = {
      mirror()
      new SmartQueue(leading.tail, trailing)
    }

    def enqueue[U >: T](x: U) = new SmartQueue[U](leading, x :: trailing)

    def toQueue: Queue[T] = Queue.newBuilder.++=(leading ++: trailing.reverse).result()
  }

  object SmartQueue {
    def apply[T](xs: T*): SmartQueue[T] = new SmartQueue(xs.toList, Nil)

    def empty[T](): SmartQueue[T] = new SmartQueue(Nil, Nil)
  }

  // apply takes S or super class of S and casts it to T or T's subclass
  trait LiskovFunction[-S, +T] {
    def apply(x: S): T
  }

  class Person(val firstName: String, val lastName: String)
    extends Ordered[Person] {
    def compare(that: Person): Int = {
      val lastNameComparison =
        lastName.compareToIgnoreCase(that.lastName)
      if (lastNameComparison != 0)
        lastNameComparison
      else
        firstName.compareToIgnoreCase(that.firstName)
    }

    override def toString: String = firstName + " " + lastName
  }

  object Person {
    def apply(firstName: String, lastName: String): Person = new Person(firstName, lastName)
  }

  /**
   * T must be a subtype of Ordered[T]
   */
  def mergeSort[T <: Ordered[T]](list: List[T]): List[T] = {
    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (leftFirst :: leftRest, rightFirst :: rightRest) => if (leftFirst < rightFirst) leftFirst :: merge(leftRest, right) else rightFirst :: merge(left, rightRest)
    }
    val middle = list.length / 2
    if (middle == 0) list
    else {
      val (left, right) = list.splitAt(middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }

}
