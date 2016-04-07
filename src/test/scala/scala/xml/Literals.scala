package scala.xml

import org.junit.Test

class Literals {

  @Test
  def begin(): Unit = {
    val elem = <a>This is some xml. Here is a tag: <atag/> </a>
    println(s"elemt = $elem")
    val curlyBraces = <a>{{in curly braces}}</a>
    println(s"curlyBraces = $curlyBraces")
  }

  @Test
  def extractText(): Unit = {
    val aTag = <a>Sounds <tag>with in tag</tag> good</a>
    println(aTag.text)
    val inOut = <a>input &gt; output</a>
    println(inOut.text)
  }

  @Test
  def searchSubelements(): Unit = {
    val abc = <a><b><c>hello</c></b></a>
    println(s"tag <b> == ${abc \ "b"}")
    // use \\ to do a deep search
    println(s"tag <c> == ${abc \\ "c"}")
    println(s"tag <a> == ${abc \\ "a"}")
    // nothing found
    println(s"tag <d> == ${abc \\ "d"}")
  }

  @Test
  def searchByAttribute(): Unit = {
    val joe = <employee name="Joe" rank="code monkey" serial="123"/> <employee name="Sarah" rank="code monkey" serial="456"/>
    println(s"joe = ${(joe \\ "@name").mkString(",")}")
    println(s"joe = ${(joe \\ "@serial").mkString(",")}")
  }

  def proc(node: Node): String = node match {
    case <a>{contents@_*}</a> => s"It's an tag a: $contents"
    case <b>{contents@_*}</b> => s"It's a tag b: $contents"
    case _ => "Its somthing else"
  }

  @Test
  def patternMatch(): Unit = {
    println(proc(<a>apple <color>red</color> <size>101</size></a>))
    println(proc(<b>banana <color>yellow</color> <size>93</size></b>))
  }

  val catalog =
    <catalog>
      <cctherm>
        <description>hot dog #5</description>
        <yearMade>1952</yearMade>
        <dateObtained>March 14, 2006</dateObtained>
        <bookPrice>2199</bookPrice>
        <purchasePrice>500</purchasePrice>
        <condition>9</condition>
      </cctherm>
      <cctherm>
        <description>Sprite Boy</description>
        <yearMade>1964</yearMade>
        <dateObtained>April 28, 2003</dateObtained>
        <bookPrice>1695</bookPrice>
        <purchasePrice>595</purchasePrice>
        <condition>5</condition>
      </cctherm>
    </catalog>

  def procCatalog(catalog: Node): Seq[String] = catalog match {
    case <catalog>{therms@_*}</catalog> => for (therm@ <cctherm>{_*}</cctherm> <- therms) yield (therm \ "description").text
  }

  @Test
  def testCatalog(): Unit = {
    assert(List("hot dog #5", "Sprite Boy") == procCatalog(catalog))
  }

}
