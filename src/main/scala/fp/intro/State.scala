package fp.intro

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  def flatMap[U >: A, B](f: U => State[S, B]): State[S, B] =
    State(state => {
      val (a, nst) = this.run(state)
      f(a).run(nst)
    })

  def map[B](f: A => B): State[S, B] = flatMap[A, B](a => State.unit(f(a)))

  def filter(p: A => Boolean): State[S, A] = this //to use syntactic sugar

  def withFilter(p: A => Boolean): State[S, A] = this

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] = flatMap[A, C](a => b.map(b => f(a, b)))

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))
}


object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = State(state => {
    val (a, sa) = s.run(state)
    f(a).run(sa)
  })


  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def many[S, A](xs: State[S, A]*): State[S, Seq[A]] = State(initState => {
    @tailrec
    def loop(as: Seq[A], state: S, sl: Seq[State[S, A]]): (Seq[A], S) = sl match {
      case Nil => (as, state)
      case x +: rs => val (xa, xstate) = x.run(state); loop(xa +: as, xstate, rs)
    }

    loop(Nil, initState, xs)
  })
}
