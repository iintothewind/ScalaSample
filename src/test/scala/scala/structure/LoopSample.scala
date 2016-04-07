package scala.structure

import java.io.File

import org.junit.Test

import scala.collection.generic.CanBuildFrom
import scala.io.Source


class LoopSample {

  @Test
  def forLoopUsingGenerator(): Unit = {
    for (file <- new File(".").list()) {
      println(file)
    }
  }

  @Test
  def forLoopUsingGeneratorWithFilter(): Unit = {
    for (file <- new File(".").listFiles()
         if file.getName.endsWith(".xml")
         if file.exists()) {
      println(file)
    }
  }

  @Test
  def forLoopNestIterationUsingGeneratorWithFilter(): Unit = {
    for (file <- new File(".").listFiles()
         if file.getName.endsWith(".xml")
         if file.exists();
         line <- Source.fromFile(file).getLines()
         if line.contains("<")) {
      printf("file: %s -> line: %s \n", file, line)
    }
  }

  @Test
  def forLoopWithYield(): Unit = {
    val tags = for {file <- new File(".").listFiles()
                    if file.getName.endsWith(".xml")
                    if file.exists()
                    line <- Source.fromFile(file).getLines()
                    trimmed = line.trim
                    if trimmed.contains("<")} yield trimmed
    tags.foreach(println)
  }

  @Test
  def forLoopUsingRange(): Unit = {
    for (i <- 1 to 4) {
      println(i)
    }
    println("until is different from to")
    for (i <- 1 until 4) {
      println(i)
    }
  }


  def makeRow(row: Int, col: Int) = for (col <- 1 to col) yield {
    val prod = (row * col).toString
    val padding = " " * (4 - prod.length)
    padding + prod
  }

  def makeTable(row: Int, col: Int) = for (currentRow <- 1 to row) yield makeRow(currentRow, col)

  @Test
  def printTable(): Unit = {
    makeTable(9, 9).foreach(row => println(row.mkString))
  }

  @Test
  def mkString(): Unit = {
    println(Array("1", "2", "3").mkString(","))
  }

  @Test
  def yieldTuple(): Unit = {
    val yielded = for (x <- List(1, 2); y <- List("one", "two")) yield (x, y)
    assert(List((1, "one"), (1, "two"), (2, "one"), (2, "two")) == yielded)
  }

  case class Book(title: String, authors: String*)

  val books: List[Book] =
    List(
      Book(
        "Structure and Interpretation of Computer Programs",
        "Abelson, Harold", "Sussman, Gerald J."
      ),
      Book(
        "Principles of Compiler Design",
        "Aho, Alfred", "Ullman, Jeffrey"
      ),
      Book(
        "Programming in Modula-2",
        "Wirth, Niklaus"
      ),
      Book(
        "Elements of ML Programming",
        "Ullman, Jeffrey"
      ),
      Book(
        "The Java Language Specification", "Gosling, James",
        "Joy, Bill", "Steele, Guy", "Bracha, Gilad"
      )
    )

  @Test
  def queryByLastName(): Unit = {
    val yielded = for (b <- books; a <- b.authors if a.startsWith("Gosling")) yield b.title
    val mapped = books.collect {
      case book if book.authors.exists(_.startsWith("Gosling")) => book.title
    }
    val collected = books.collect({
      case book if book.authors.exists(_.startsWith("Gosling")) => book.title -> book
    })(new CanBuildFrom[List[Book], (String, Book), Map[String, Book]] {
      override def apply(from: List[Book]) = apply()

      override def apply() = Map.newBuilder[String, Book]
    })
    println(collected)
    assert(mapped == yielded)
  }

  @Test
  def queryByTitle(): Unit = {
    val yielded = for (b <- books if b.title.startsWith("Program")) yield b.title
    val collected = books.collect({
      case book if book.title.startsWith("Program") => book.title
    })
    assert(collected == yielded)
  }

  @Test
  def queryForSameAuthor(): Unit = {
    val yielded = for (b1 <- books; b2 <- books if b1 != b2; a1 <- b1.authors; a2 <- b2.authors if a1 == a2) yield a1
    val filtered = books.flatMap(b1 =>
      books.withFilter(b2 => b1 != b2).flatMap(b2 =>
        b1.authors.flatMap(a1 =>
          //b2.authors.withFilter(a2 => a1 == a2).map(identity(_))
          b2.authors.filter(a2 => a1 == a2)
        )
      )
    )
    assert(filtered.toSet == yielded.toSet)
  }


}
