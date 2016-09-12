package fp.design.combinator

import org.scalacheck.Properties
import org.scalacheck.Prop._


object StringSpecification extends Properties("String") {
  property("startsWith") = forAll { (a: String, b: String) => (a + b).startsWith(a) }

  property("concat") = forAll { (a: String, b: String) => a.concat(b).length >= a.length && a.concat(b).length >= b.length }
}

class PropCheckSample {

}
