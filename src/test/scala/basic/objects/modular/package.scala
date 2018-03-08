package basic.objects

package object modular {

  abstract class Food(val name: String) {
    override def toString = name
  }

  object NonFood extends Food("")

  trait SimpleFoods {

    object Apple extends Food("Apple")

    object Orange extends Food("Orange")

    object Cream extends Food("Cream")

    object Sugar extends Food("Sugar")

    object Pear extends Food("Pear")

    def allFoods = List(Apple, Orange, Cream, Sugar, Pear)
  }

  class Recipe(val name: String, val ingredients: List[Food], val instructions: String) {
    override def toString: String = name
  }

  trait SimpleRecipes {
    this: SimpleFoods =>

    object FruitSalad extends Recipe(
      "fruit salad",
      List(Apple, Pear),
      "Mix it all tegether"
    )

    def allRecipes = List(FruitSalad)
  }

  trait FoodCategories {

    case class FoodCategory(name: String, foods: List[Food])

    def allCategories: List[FoodCategory]
  }

  abstract class Database extends FoodCategories {
    def foodNamed(name: String): Option[Food] = allFoods.find(_.name == name)

    def allFoods: List[Food]

    def allRecipes: List[Recipe]
  }

  abstract class Browser {
    val database: Database

    def recipeUsing(food: String): List[Recipe] = database.allRecipes.filter(_.ingredients.contains(database.foodNamed(food).getOrElse(NonFood)))

    def recipeUsing(food: Food): List[Recipe] = database.allRecipes.filter(_.ingredients.contains(food))

    def displayCategory(category: database.FoodCategory): Unit = println(category)
  }

}
