package scala

package object convert {
  implicit def name = "there"

  implicit def poorOrdering[T](left: T, right: T): Boolean = String.valueOf(left) > String.valueOf(right)

  implicit final class MagicWand[T](private val self: T) extends AnyVal {
    @inline
    def <->[B](target: B): String = String.valueOf(self) + String.valueOf(target) * 2 + String.valueOf(self)
  }

  class IdPig(val id: Int) extends Ordered[IdPig] {
    override def compare(that: IdPig): Int = this.id - that.id

    override def toString: String = "pig" + this.id
  }

  object IdPig {
    def apply(id: Int): IdPig = new IdPig(id)
  }

}
