package basic.objects

package object stateful {

  class SelfTime {
    var hour = 12
    var minute = 0
  }

  class CustomTime {
    //private[this] means it can be accessed only from the object that contains it
    private[this] var h = 12
    private[this] var m = 0

    def hour: Int = h

    def hour_=(x: Int): Unit = {
      require(0 <= x && x < 24)
      h = x
    }

    def minute: Int = m

    def minute_=(x: Int): Unit = {
      require(0 <= x && x < 60)
      m = x
    }
  }

  class Thermometer {
    //set to a default value by specifying _ as the initializing value of the variable
    //an initializer = _ of a field assigns a zero value to that field
    var celsius: Float = _

    override def toString: String = s"$fahrenheit  F/ $celsius C"

    def fahrenheit: Float = celsius * 9 / 5 + 32

    def fahrenheit_=(f: Float): Unit = {
      celsius = (f - 32) * 5 / 9
    }
  }

}
