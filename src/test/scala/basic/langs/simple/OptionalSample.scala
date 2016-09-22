package basic.langs.simple

import org.junit.Test

class OptionalSample {

  @Test(expected = classOf[NoSuchElementException])
  def testGetOrElse(): Unit = {
    val x: Option[String] = None
    assert("default" == x.getOrElse("default"))
    x.get
  }

  @Test
  def testGetNoneForNullInput(): Unit = {
    val x: Option[String] = Option(null)
    assert(x match {
      case None => true
      case otherwise => false
    })
  }

  @Test
  def testGetSomeForNonNull(): Unit = {
    val x: Option[String] = Option("initialized")
    assert(x match {
      case Some(s) => s == "initialized"
      case otherwise => false
    })
  }

  @Test
  def testExtractByForeach(): Unit = {
    val server: Option[String] = Option("http://any-server.org/")
    val username: Option[String] = None
    val password: Option[String] = Option("passwd")

    server.foreach(s => println(s"server address: $s"))
    username.foreach(s => println(s"login username: $s"))
    password.foreach(s => println(s"login password: $s"))
  }

  @Test
  def testExtractByFor(): Unit = {
    val server: Option[String] = Option("http://any-server.org/")
    val username: Option[String] = None
    val password: Option[String] = Option("passwd")

    for (s <- server; u <- username; p <- password) {
      println(s"server address: $s")
      println(s"login username: $u")
      println(s"login password: $p")
    }
  }

  @Test
  def testYieldNone(): Unit = {
    val server: Option[String] = Option("http://any-server.org/")
    val username: Option[String] = None
    val password: Option[String] = Option("passwd")

    val connection = for {s <- server
                          u <- username
                          p <- password} yield (s, u, p)

    assert(connection match {
      case None => true
      case Some(x) => println(s"server: ${x._1}, username: ${x._2}, password: ${x._3}"); false
      case otherwise => false
    })
  }

  @Test
  def testExtractValue3(): Unit = {
    val server: Option[String] = Option("http://any-server.org/")
    val username: Option[String] = Option("usr")
    val password: Option[String] = Option("passwd")
    // use for yield to create a new Option[(String,String,String)]
    val connection = for {s <- server
                          u <- username
                          p <- password} yield (s, u, p)

    assert(connection match {
      case Some(x) => println(s"server: ${x._1}, username: ${x._2}, password: ${x._3}"); true
      case otherwise => false
    })

  }

}
