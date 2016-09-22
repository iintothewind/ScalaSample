package basic.objects.basic

import java.util.Objects

import org.junit.Test

import scala.collection.immutable.HashSet

/**
 * 1. It is reflexive: for any non-null value x , the expression x.equals(x)
  should return true.
 * 2. It is symmetric: for any non-null values x and y, x.equals(y) should
  return true if and only if y.equals(x) returns true.
 * 3. It is transitive: for any non-null values x, y, and z, if x.equals(y) returns true and y.equals(z) returns true, then x.equals(z) should
  return true.
 * 4. It is consistent: for any non-null values x and y, multiple invocations
  of x.equals(y) should consistently return true or consistently return false, provided no information used in equals comparisons on
  the objects is modified.
 * 5. For any non-null value x, x.equals(null) should return false
 */
class Equality {

  class PointWithWrongEquals(val x: Int, val y: Int) {
    def equals(that: PointWithWrongEquals): Boolean = this.x == that.x && this.y == that.y
  }

  @Test
  def defineEqualsWithWrongSignature(): Unit = {
    val p1, p2 = new PointWithWrongEquals(1, 2)
    val q = new PointWithWrongEquals(2, 3)
    assert(p1.equals(p2))
    assert(!p1.equals(q))
    val coll = HashSet(p1)
    // false because PointWithWrongEquals.equals has a wrong signature
    // it should be override def equals(obj: scala.Any): Boolean
    assert(!coll.contains(p2))
  }

  class PointWithoutOverridingHashCode(val x: Int, val y: Int) {
    override def equals(that: Any): Boolean = that match {
      case that: PointWithoutOverridingHashCode => this.x == that.x && this.y == that.y
      case _ => false
    }
  }

  @Test
  def withoutOverridingHashCode(): Unit = {
    val p1, p2 = new PointWithoutOverridingHashCode(1, 2)
    assert(p1 == p2)
    val coll = HashSet(p1)
    assert(p1.hashCode != p2.hashCode)
    // coll.contains(p2) returns false cause p1.hashCode != p2.hashCode
    assert(!coll.contains(p2))
  }

  class PointWithMutableFields(var x: Int, var y: Int) {
    override def hashCode: Int = Objects.hash(x.asInstanceOf[Integer], y.asInstanceOf[Integer])

    override def equals(that: Any): Boolean = that match {
      case that: PointWithoutOverridingHashCode => this.x == that.x && this.y == that.y
      case _ => false
    }
  }

  /**
  If they put such objects into collections, they have to be careful never to
      modify the depended-on state, and this is tricky. If you need a comparison
      that takes the current state of an object into account, you should usually name
      it something else, not equals.
    */
  @Test
  def withMutableFields(): Unit = {
    val p = new PointWithMutableFields(1, 2)
    val coll = HashSet(p)

    assert(coll.contains(p))
    p.x += 1
    // hashCode has been changed due to the change of p.x
    assert(!coll.contains(p))
    // List uses equals to check contains() therefore hashCode change will not affect equals
    assert(coll.toList.contains(p))
  }

  object Color extends Enumeration {
    val Red, Orange, Yellow, Green, Blue, Indigo, Violet = Value
  }

  class Point(val x: Int, val y: Int) {
    override def hashCode: Int = Objects.hash(x.asInstanceOf[Integer], y.asInstanceOf[Integer])

    override def equals(that: Any): Boolean = that match {
      case that: Point => this.x == that.x && this.y == that.y
      case _ => false
    }
  }

  class ColoredPointAsymmetric(override val x: Int, override val y: Int, val color: Color.Value) extends Point(x, y) {
    override def equals(that: Any): Boolean = that match {
      case that: ColoredPointAsymmetric => this.color == that.color && super.equals(that)
      case _ => false
    }
  }

  class ColoredPointSymmetric(override val x: Int, override val y: Int, val color: Color.Value) extends Point(x, y) {
    override def equals(that: Any): Boolean = that match {
      case that: ColoredPointSymmetric => this.color == that.color && super.equals(that)
      case that: Point => that.equals(this)
      case _ => false
    }
  }

  /**
   * equals should be symmetric: for any non-null values x and y, x.equals(y) should return true if and only if y.equals(x) returns true.
   */
  @Test
  def equalsOnAsymmetric(): Unit = {
    val point = new Point(1, 2)
    val coloredPointAsymmetric = new ColoredPointAsymmetric(1, 2, Color.Red)
    assert(point.equals(coloredPointAsymmetric))
    // coloredPointAsymmetric.equals() would take color to account, other case returns false
    assert(!coloredPointAsymmetric.equals(point))
  }

  @Test
  def equalsOnSymmetric(): Unit = {
    val point = new Point(1, 2)
    val coloredPointSymmetric = new ColoredPointSymmetric(1, 2, Color.Red)
    assert(point.equals(coloredPointSymmetric))
    assert(coloredPointSymmetric.equals(point))
  }

  /**
   * equals should be transitive: for any non-null values x, y, and z, if x.equals(y) returns true and y.equals(z) returns true, then x.equals(z) should return true.
   */
  @Test
  def nonTransitive(): Unit = {
    val point = new Point(1, 2)
    val redPoint = new ColoredPointSymmetric(1, 2, Color.Red)
    val bluePoint = new ColoredPointSymmetric(1, 2, Color.Blue)
    assert(point == redPoint && redPoint == point)
    assert(point == bluePoint && bluePoint == point)
    // the example is not proper, since redPoint indeed should not be equal to bluePoint
    assert(!(redPoint == bluePoint))
  }

  class ColoredPoint(override val x: Int, override val y: Int, val color: Color.Value) extends Point(x, y) {
    override def equals(that: Any): Boolean = that match {
      case that: ColoredPoint => this.color == that.color && super.equals(that)
      case that: Point => that.equals(this)
      case _ => false
    }
  }

  /**
   * leave this unimplemented
   */
  @Test
  def transitive(): Unit = {
    val point = new Point(1, 2)
    val redPoint = new ColoredPoint(1, 2, Color.Red)
    val bluePoint = new ColoredPoint(1, 2, Color.Blue)
    assert(point == redPoint && redPoint == point)
    assert(point == bluePoint && bluePoint == point)
  }

  trait Tree[+T] {
    def element: T

    def left: Tree[T]

    def right: Tree[T]
  }

  object EmptyNode extends Tree[Nothing] {
    def element = throw new NoSuchElementException("EmptyTree.elem")

    def left = throw new NoSuchElementException("EmptyTree.left")

    def right = throw new NoSuchElementException("EmptyTree.right")
  }

  class Node[+T](override val element: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
    override def equals(that: Any): Boolean = that match {
      case that: Node[T] => this.element == that.element && this.left == that.left && this.right == that.right
      case _ => false
    }
  }

  @Test
  def equalsForGenericType(): Unit = {
    val one = new Node(1, EmptyNode, EmptyNode)
    val anotherOne = new Node(1, EmptyNode, EmptyNode)
    assert(one == anotherOne)
    // compare with two parameterized types will still return true, cause generic type would be erased after compilation
    assert(new Node[List[Int]](Nil, EmptyNode, EmptyNode) == new Node[List[String]](Nil, EmptyNode, EmptyNode))
  }


}
