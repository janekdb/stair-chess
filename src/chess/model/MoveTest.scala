package chess.model

import test.{ Main, Test, TestUtils }

object MoveTest extends Test with TestUtils with Main {

  // TODO: Remove parens on all runTests
  def runTests {
    rejectWhenStartEqualsEnd
    enPassant
  }

  private def rejectWhenStartEqualsEnd {
    assertExceptionThrown("A move with the start equal to the end position should be rejected", classOf[IllegalArgumentException]) {
      val p = new Position("e7")
      MovePiece(p, p)
    }
    assertExceptionThrown("A move with the start equal to the end position should be rejected", classOf[IllegalArgumentException]) {
    	val p = new Position("e7")
    	MovePieceCapturing(p, p)
    }
  }

  private def enPassant {
    val e = EnPassant("e5", "d6")
    assertEquals(new Position("d5"), e.taken)
  }

}