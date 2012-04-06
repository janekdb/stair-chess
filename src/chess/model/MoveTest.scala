package chess.model

import test.{Main, Test, TestUtils}

object MoveTest extends Test with TestUtils with Main {

  def runTests() {
    enPassant
  }

  private def enPassant = {
    val e = EnPassant("e5", "d6")
    assertEquals(new Position("d5"), e.taken)
  }

}