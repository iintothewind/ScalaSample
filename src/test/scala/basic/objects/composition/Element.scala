package basic.objects.composition

object Element {

  private class ArrayElement(val contents: Array[String]) extends Element

  private class LineElement(s: String) extends Element {
    override def contents = Array(s)

    override def width = s.length

    override def height = 1
  }

  private class UniformElement(ch: Char, override val width: Int, override val height: Int) extends Element {
    override def contents = Array.fill(height)(ch.toString * width)
  }

  def elem(contents: Array[String]): Element = new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element = new UniformElement(chr, width, height)

  def elem(line: String): Element = new LineElement(line)
}

import Element.elem

abstract class Element {
  //parameterless methods
  //recommended convention is to use a parameterless method whenever there are no parameters  and method access mutable state,
  //only by reading fields of the containing object
  //compared with field access method, parameterless method costs less memory, but runs slightly slower

  // defers from def content():Array[String] = {}
  def contents: Array[String]

  // defers from def width():Int
  def width: Int = contents(0).length

  // defers from def height():Int
  def height: Int = contents.length


  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(for ((line1, line2) <- this1.contents zip that1.contents) yield line1 + line2)
  }

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val right = elem(' ', w - width, height)
      this beside right
    }

  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val bot = elem(' ', width, h - height)
      this above bot
    }

  override def toString = contents mkString "\n"
}
