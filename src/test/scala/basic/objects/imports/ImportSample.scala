package basic.objects.imports

import basic.objects.imports
import org.junit.Test

abstract class Fruit(val name: String, val color: String)

object Fruits {

  object Apple extends Fruit("apple", "red")

  object Orange extends Fruit("orange", "orange")

  object Pear extends Fruit("pear", "yellowish")

  val menu = List(Apple, Orange, Pear)
}


class PackagesSample {

  @Test
  def testPackageObject(): Unit = println(imports.name)

  @Test
  def testImportAllMembers(): Unit = {
    import com.google.common.base.Preconditions._
    checkArgument(imports.name.contains("package"), "%s does not contains package", "package object")
    checkNotNull(imports.name, "%s is null", "package object")
  }


  @Test
  def testImportMembersOfRegularObject(): Unit = {
    println("not supported by IntelliJ 14")
  }

  @Test
  def testImportSelected(): Unit = {
    import com.google.common.base.Preconditions.{checkArgument, checkNotNull}
    checkArgument(imports.name.contains("package"), "%s does not contains package", "package object")
    checkNotNull(imports.name, "%s is null", "package object")
  }

  @Test
  def testImportRenaming(): Unit = {
    import Fruits.{Apple => A}
    println(A.name)
  }

  @Test
  def testImportExcept(): Unit = {
    import Fruits.{Pear => _, _}
    println(Apple.name)
    println(Orange.name)
    // println(Pear.name) wont compile, cause Orange is excluded
  }

}
