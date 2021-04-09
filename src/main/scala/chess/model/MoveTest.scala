package chess.model

import test.{ Main, Test, TestUtils }

object MoveTest extends Test with TestUtils with Main {

  def runTests: Unit = {
    rejectWhenStartEqualsEnd
    enPassant
  }

  private def rejectWhenStartEqualsEnd: Unit = {
    assertExceptionThrown("A move with the start equal to the end position should be rejected", classOf[IllegalArgumentException]) {
      val p = new Position("e7")
      MovePiece(p, p)
    }
    assertExceptionThrown("A move with the start equal to the end position should be rejected", classOf[IllegalArgumentException]) {
      val p = new Position("e7")
      MovePieceCapturing(p, p)
    }
  }

  private def enPassant: Unit = {
    val e = EnPassant("e5", "d6")
    assertEquals(new Position("d5"), e.captured)
  }

  private def promote: Unit = {
    val wp = Promote("a7", Queen)
    assertEquals(wp.end, new Position("a8"), "The end position of a Promote was correct for White")
    val bp = Promote("a2", Queen)
    assertEquals(bp.end, new Position("a1"), "The end position of a Promote was correct for Black")
  }
}