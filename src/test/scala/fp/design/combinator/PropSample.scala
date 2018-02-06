package fp.design.combinator

sealed trait Prop {
  def check: Boolean = false
  def &&(p: Prop): Prop = ???
}

class PropSample {

}
