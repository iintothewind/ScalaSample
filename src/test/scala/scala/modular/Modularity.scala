package scala.modular

import org.junit.Test

class Modularity {

  object SimpleDatabase extends Database with SimpleFoods with SimpleRecipes {
    override def allCategories = List(
      FoodCategory("fruit", List(Apple, Orange, Pear)),
      FoodCategory("misc", List(Cream, Sugar))
    )
  }

  object FrozenFood extends Food("FrozenFood")

  object HeatItUp extends Recipe(
    "heat it up",
    List(FrozenFood),
    "Microwave the 'food' for 10 minutes"
  )


  object StudentDatabase extends Database {
    override def allFoods: List[Food] = List(FrozenFood)

    override def allCategories = List(FoodCategory("forzen", List(FrozenFood)))

    override def allRecipes: List[Recipe] = List(HeatItUp)
  }

  object SimpleBrowser extends Browser {
    override val database: Database = SimpleDatabase
  }

  @Test
  def testSimpleBrowser(): Unit = {
    println(SimpleBrowser.recipeUsing("Apple"))
  }


}
