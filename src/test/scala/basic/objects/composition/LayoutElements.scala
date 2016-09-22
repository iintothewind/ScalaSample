package basic.objects.composition

import org.junit.Test
import Element.elem

class LayoutElements {
  @Test
  def constructElements(): Unit = {
    val column1 = elem("UserName") above elem("Ivar") above elem("Ashley")
    val column2 = elem("Password") above elem("scala")
    val column3 = elem("Occupation") above elem("") above elem("Designer")
    println(column1 beside column2 beside column3)
  }

  @Test
  def spiral(): Unit = {
    print(Spiral.spiral(12, 0))
  }


}
