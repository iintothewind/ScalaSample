package fp.intro

import org.junit.Test

sealed case class CreditCard(private val balance: Double) {
  def apply(balance: Double): CreditCard = new CreditCard(balance)

  def charge(price: Double): CreditCard = {
    this (balance - price)
  }
}


sealed case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new RuntimeException("Can't combine charges to different cards")
  }
}

sealed case class Coffee(name: String = "Mocha", price: Double = 3)

object Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cafe = Coffee()
    (cafe, Charge(cc, cafe.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (Seq[Coffee], Charge) = {
    val purchases = Seq.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

class CafeSample {
  @Test
  def testBuyCoffee(): Unit = {
    assert((Coffee(), Charge(CreditCard(99), 3)) == Cafe.buyCoffee(CreditCard(99)))
  }

  @Test
  def testBuyCoffees(): Unit = {
    assert((Seq.fill(9)(Coffee()), Charge(CreditCard(99), Coffee().price * 9)) == Cafe.buyCoffees(CreditCard(99), 9))
  }

}
