package scala.xml

import java.io.File

import org.junit.{Ignore, Test}

class Serialize {
  abstract class CCTherm {
    val description: String
    val yearMade: Int
    val dateObtained: String
    val bookPrice: Int
    val purchasePrice: Int
    val condition: Int

    override def toString = description

    def toXml =
      <cctherm>
        <description>
          {description}
        </description>
        <yearMade>
          {yearMade}
        </yearMade>
        <dateObtained>
          {dateObtained}
        </dateObtained>
        <bookPrice>
          {bookPrice}
        </bookPrice>
        <purchasePrice>
          {purchasePrice}
        </purchasePrice>
        <condition>
          {condition}
        </condition>
      </cctherm>
  }

  val node =
    <cctherm>
      <description>
        Aurora 001
      </description>
      <yearMade>
        2014
      </yearMade>
      <dateObtained>
        Mar 14th, 2015
      </dateObtained>
      <bookPrice>
        159
      </bookPrice>
      <purchasePrice>
        500
      </purchasePrice>
      <condition>
        3
      </condition>
    </cctherm>

  object CCTherm {
    def fromXml(node: Node): CCTherm = new CCTherm {
      override val condition: Int = (node \ "condition").text.trim.toInt
      override val description: String = (node \ "description").text.trim
      override val bookPrice: Int = (node \ "bookPrice").text.trim.toInt
      override val purchasePrice: Int = (node \ "purchasePrice").text.trim.toInt
      override val yearMade: Int = (node \ "yearMade").text.trim.toInt
      override val dateObtained: String = (node \ "dateObtained").text.trim
    }
  }

  @Test
  def serialization(): Unit = {
    val therm = new CCTherm {
      override val condition: Int = 9
      override val description: String = "hot dog #5"
      override val bookPrice: Int = 2199
      override val purchasePrice: Int = 500
      override val yearMade: Int = 1952
      override val dateObtained: String = "Mar 14th, 2015"
    }
    println(s"therm.toXml = ${therm.toXml}")
  }

  @Test
  def deserialization(): Unit = {
    assert("Aurora 001" == CCTherm.fromXml(node).description)
  }

  @Test
  def save(): Unit = {
    XML.save("./target/cctherm.xml", node)
    //io.Source.fromFile("./target/cctherm.xml").getLines().foreach(println)
  }

  @Test
  def load(): Unit = {
    XML.save("./target/cctherm.xml", node)
    assert(node == XML.load("./target/cctherm.xml"))
  }

}
