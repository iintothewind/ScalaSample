package scala

// package object name should be the same as packages's name where package object is located in
package object imports {
  def name: String = this.getClass.getCanonicalName

}
