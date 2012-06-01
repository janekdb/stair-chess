package chess.model

import Colours.{ Black, White }

import test.{Main, Test, TestUtils}

object GridConfigurationTest extends Test with TestUtils with Main {

  def runTests {
    moveHistoryMaintained
    moveHistoryCopied
    enPassantEventSent
    pieceMovedEventSent
    pieceMovedCapturingEventSent
    promoteEventsSent
    promoteCapturingEventsSent
    promoteReplacesPiece
  }

  def moveHistoryMaintained {
    val conf = new GridConfiguration

    val whiteStart: Position = "e2"
    val whiteEnd: Position = "e3"
    conf.add(whiteStart, White, Pawn())

    val blackStart: Position = "h8"
    val blackEnd: Position = "h4"
    conf.add(blackStart, Black, Rook())

    conf.getLastMove match {
      case None => Unit
      case default => fail("The last move should have been None")
    }

    conf.applyMove(MovePiece(whiteStart, whiteEnd))
    assertSomeEquals((Pawn(), whiteStart, whiteEnd), conf.getLastMove)

    conf.applyMove(MovePiece(blackStart, blackEnd))
    assertSomeEquals((Rook(), blackStart, blackEnd), conf.getLastMove)
  }

  def moveHistoryCopied {
    val conf = new GridConfiguration

    val whiteStart: Position = "e2"
    val whiteEnd: Position = "e3"
    conf.add(whiteStart, White, Pawn())

    conf.applyMove(MovePiece(whiteStart, whiteEnd))
    val copy = conf.copyOf
    assertSomeEquals((Pawn(), whiteStart, whiteEnd), copy.getLastMove)
  }

  def assertSomeEquals(expected: (Piece, Position, Position), actual: Option[(Piece, Position, Position)]) {
    actual match {
      case Some((_, _, _)) => Unit
      case default => fail("The last move should have been Some((_, _)) but was " + actual)
    }
    val Some((p, a, b)) = actual
    assertEquals(expected, (p, a, b))
  }

  def enPassantEventSent {
    val conf = new GridConfiguration

    val whiteStart: Position = "a5"
    val whiteEnd: Position = "b6"
    conf.add(whiteStart, White, Pawn())

    val blackStart: Position = "b7"
    val blackEnd: Position = "b5"
    conf.add(blackStart, Black, Pawn())

    conf.applyMove(MovePiece(blackStart, blackEnd))
    val events = conf.applyMove(EnPassant(whiteStart, whiteEnd))
    events match {
      case List(PieceMovedCapturing(start, end, captured)) => {
        assertEquals(whiteStart, start)
        assertEquals(whiteEnd, end)
        assertEquals(blackEnd, captured)
      }
      case default => fail("Unexpected list of events: " + events)
    }
    val (colour, piece, _) = conf.getExistingPiece(whiteEnd)
    assertEquals(White, colour)
    assertEquals(Pawn(), piece)
    conf.getPiece(blackEnd) match {
      case None => {}
      case default => fail("The black pawn should have been taken")
    }
  }

  def pieceMovedEventSent { // TODO: Implement pieceMovedEventSent
  }
  def pieceMovedCapturingEventSent { // TODO: Implement pieceMovedCapturingEventSent
  }
  def promoteEventsSent { // TODO: Implement promoteEventsSent
  }
  def promoteCapturingEventsSent { // TODO: Implement promoteCapturingEventsSent
  }

  def promoteReplacesPiece {
    val conf = new GridConfiguration

    val start: Position = "f7"
    val end: Position = "f8"
    conf.add(start, White, Pawn())
    val events = conf.applyMove(Promote(start, end, Knight()))
    assertEquals(PieceMoved(start, end) :: Promoted(end, Knight()) :: Nil, events)
    assertEquals(List(), conf.locatePieces(White, Pawn()), "There should not have been any pawns")
    assertEquals(List(end), conf.locatePieces(White, Knight()), "A knight should be present")
  }
}