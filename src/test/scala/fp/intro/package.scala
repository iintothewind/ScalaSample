package fp

package object intro {
  type Rand[+A] = State[Rng, A]
}
