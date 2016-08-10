package fp

package object intro {
  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[Rng, A]
}
