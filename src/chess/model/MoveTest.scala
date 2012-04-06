package chess.model

import test.{Test, TestUtils}

object MoveTest extends Test with TestUtils {

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