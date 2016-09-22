package basic.objects.composition

import org.junit.Test

import scala.languageFeature.existentials

class ExistentialTypes {

  trait A[T]

  class AI extends A[Int]

  class AS extends A[String]

  def foo(as: Seq[A[X] forSome {type X}]) = true

  def bar[X](as: Seq[A[X]]) = Set.empty[X]


  @Test
  def forSomes(): Unit = {
    // Iterator<?> in java would be
    foo(Seq(new AI, new AS))
  }

  def fooExt(as: Seq[A[X] forSome {type X <: Iterable[Int]}]) = true

  def barExt[X <: Iterable[String]](as: Seq[A[X]]) = Set.empty[X]

  class AItrInt extends A[Seq[Int]]

  class AItrString extends A[List[String]]

  @Test
  def forSomeExtends(): Unit = {
    fooExt(Seq(new AItrInt))
    barExt(List(new AItrString))
  }


}
