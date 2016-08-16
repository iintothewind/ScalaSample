package fp.design.combinator


sealed trait Par[+A] {
  def get: A

  def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C]
}

object Par {
  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def run[A](par: Par[A]): A = ???

  def lazyUnit[A](a: => Par[A]): Par[A] = ???

  def get[A](par: Par[A]): A = ???

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
}

class ParSample {

}
