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

    def assertEquals(expected: (Position, Position), actual: Option[(Position, Position)]) = {
      actual match {
        case Some((_, _)) => Unit
        case default => fail("The last move should have been Some((_, _)) but was " + conf.getLastMove)
      }
      assert(expected == actual)
    }

    conf.move(whiteStart, whiteEnd)
    assertEquals((whiteStart, whiteEnd), conf.getLastMove)

    conf.move(blackStart, blackEnd)
    assertEquals((blackStart, blackEnd), conf.getLastMove)
  }

  def moveHistoryCopied = fail

}