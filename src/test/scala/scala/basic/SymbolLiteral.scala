package scala.basic

import org.junit.Test

class SymbolLiteral {

  def updateRecordByName(symbol: Symbol, value: Any): Unit = {
    println(String.format("symbol.name == %s, value == %s", symbol.name, value.toString))
  }

  @Test
  def showSymbolLiteral(): Unit = {
    updateRecordByName('favoriteAlbum, "OK Parameter")
  }

}
