package basic.objects.traits

import org.junit.Test

import scala.collection.mutable.ArrayBuffer

trait Animal

trait Philosophical {
  def philosophize(): Unit = {
    println("I consume memory, therefore I am!")
  }
}

// abstract modifier can be used only for classes
// it should be omitted for abstract members for traits
trait Mouth {
  // abstract methods, abstract modifier should be omitted
  def speak(words: String): Unit

  def eat(food: String): Unit
}

trait HasLegs {

  def walk(): Unit = println("I can walk")


  def run(): Unit = println("I can run")


  def jump(): Unit = println("I can jump")

}

/**
  * Mixin a trait using extends, Mixin more than one trait using extends then with
  * when you call a method on a class with multiple trait mixins,
  * the method in the trait furthest to the right is called first,
  * if that method calls super, it invokes the method in the next trait to its left, and so on
  */
class TraitSample {


  @Test
  def testFrog(): Unit = {
    val frog = new Frog

    val phil: Philosophical = frog
    phil.philosophize()

    val animalWithLegs: HasLegs = frog
    animalWithLegs.run()
  }

  @Test
  def withMouth(): Unit = {
    val withMouth = new Dog
    withMouth.speak("hello")
    withMouth.eat("bones")
  }

  @Test
  def testBasicIntQueue(): Unit = {
    val queue = new BasicIntQueue
    queue.put(10)
    queue.put(20)
    assert(10 == queue.get())
    assert(20 == queue.get())
  }

  @Test
  def testWithDoubling(): Unit = {
    val doublingQueue = new DoublingQueue
    doublingQueue.put(10)
    assert(20 == doublingQueue.get())
  }

  @Test
  def testWithDoublingIncrementing(): Unit = {
    val queue = new IncrementingDoublingQueue
    queue.put(10)
    assert(22 == queue.get())
  }

  // but you cannot define trait with parameters
  // trait NoPoint(x:Int, y:Int)


  //super calls are statically bound in class
  //super calls are dynamically bound in trait

  @Test
  def testWithDoublingIncrementingQueue(): Unit = {
    val queue = new DoublingIncrementingQueue
    queue.put(10)
    assert(21 == queue.get())
  }

  @Test
  def testFilteringDoublingIncrementingQueue(): Unit = {
    val queue = new FilteringDoublingIncrementingQueue
    queue.put(-10)
    assert(1 == queue.get())
  }

  @Test
  def testCat(): Unit = {
    val cat = new Cat
    println(cat.toString)
    println(cat.fur)
    cat.walk()
    cat.run()
    cat.jump()
  }

  // Doubling extends IntQueue
  // it means that trait can only be mixed into a class that also exntends IntQueue
  trait Doubling extends IntQueue {
    abstract override def put(x: Int): Unit = {
      // super can only be used when its method is overridden by a member declared abstract and override
      super.put(2 * x)
    }
  }

  trait Incrementing extends IntQueue {
    abstract override def put(x: Int): Unit = super.put(x + 1)
  }

  trait Filtering extends IntQueue {
    abstract override def put(x: Int): Unit = if (x > 0) super.put(x) else super.put(0)
  }

  trait Furry extends Animal {
    def fur: String = "I am furry"
  }

  trait FourLegged extends HasLegs {
    override def walk(): Unit = {
      super.walk()
      println("with four legs")
    }

    override def run(): Unit = {
      super.run()
      println("with four legs")
    }

    override def jump(): Unit = {
      super.jump()
      println("with four legs")
    }
  }

  abstract class IntQueue {
    def get(): Int

    def put(x: Int): Unit
  }

  class Frog extends Animal with Philosophical with HasLegs {
    override def toString = "green"

    override def run(): Unit = super.jump()

  }

  class Dog extends Animal with Mouth {
    override def speak(words: String): Unit = println("wang wang, " + words)

    override def eat(food: String): Unit = println("eat " + food)
  }

  // you can define a class like this
  class Point(x: Int, y: Int)

  class BasicIntQueue extends IntQueue {
    private val buf = new ArrayBuffer[Int]()

    def get(): Int = buf.remove(0)

    def put(x: Int): Unit = buf += x
  }

  //when you call a method on a class with mixins,
  // the method in the trait furthest to the right is called first
  // if that method calls super, it invokes the method in the next trait to its left, and so on
  class DoublingQueue extends BasicIntQueue with Doubling

  class IncrementingDoublingQueue extends BasicIntQueue with Doubling with Incrementing

  class DoublingIncrementingQueue extends BasicIntQueue with Incrementing with Doubling

  class FilteringDoublingIncrementingQueue extends BasicIntQueue with Incrementing with Doubling with Filtering

  class Cat extends Animal with Furry with FourLegged

}
