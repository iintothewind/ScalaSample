package scala

import scala.language.implicitConversions

package object objects {
  implicit def intToRational(i: Int): Rational = new Rational(i)
}
