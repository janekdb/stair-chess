package chess.model

import Colours.{ Black, White }

import test.Test

object GridConfigurationTest extends Test {

  // TODO: Define this test helping implicits in a common location
  implicit def piece2List(t: Piece) = List(t)
  implicit def string2Position(s: String) = new Position(s)
  // TODO: Replace new MovePiece(x) with x
  //  implicit def string2MovePiece(s: String) = new MovePiece(s)

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {
    moveHistoryMaintained
    moveHistoryCopied
    enPassantEventSent
  }

  def moveHistoryMaintained = {
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

  def moveHistoryCopied = {
    val conf = new GridConfiguration

    val whiteStart: Position = "e2"
    val whiteEnd: Position = "e3"
    conf.add(whiteStart, White, Pawn())

    conf.applyMove(MovePiece(whiteStart, whiteEnd))
    val copy = conf.copyOf
    assertSomeEquals((Pawn(), whiteStart, whiteEnd), copy.getLastMove)
  }

  def assertSomeEquals(expected: (Piece, Position, Position), actual: Option[(Piece, Position, Position)]) = {
    actual match {
      case Some((_, _, _)) => Unit
      case default => fail("The last move should have been Some((_, _)) but was " + actual)
    }
    val Some((p, a, b)) = actual
    assertEquals(expected, (p, a, b))
  }

  def enPassantEventSent = {
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
      case List(PieceMovedTaking(start, end, taken)) => {
        assertEquals(whiteStart, start)
        assertEquals(whiteEnd, end)
        assertEquals(blackEnd, taken)
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
}