package chess.model

import test.{Test, TestUtils}

object MoveTest extends Test with TestUtils {

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]) {
    runTests
  }

  def runTests() {
    enPassant
  }

  private def enPassant = {
    val e = EnPassant("e5", "d6")
    assertEquals(new Position("d5"), e.taken)
  }

}