package scala.stateful

import org.junit.Test

class Reassignable {
  @Test
  def getterAndSetter(): Unit = {
    //every var that is a non-private member of some object
    // implicitly defines a getter and a setter method with it
    //The getter of a var x is just named “x”, while its setter is named “x_=”
    val selfTime = new SelfTime
    selfTime.hour = 9
    selfTime.minute = 30
    println(selfTime.hour + ":" + selfTime.minute)
    val customTime = new CustomTime
    customTime.hour_=(12)
    customTime.hour = 16
    customTime.minute = 30
    println(customTime.hour + ":" + customTime.minute)
  }

  @Test
  def withoutAssociatedField(): Unit = {
    val thermometer = new Thermometer
    println(thermometer.fahrenheit)
    thermometer.fahrenheit = 97f
    println(thermometer)
    thermometer.celsius = 36.5f
    println(thermometer)
  }

}
