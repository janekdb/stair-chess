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
    var conf = new GridConfiguration
    conf.add("e2", White, Pawn())
    conf.add("h8", Black, Rook())

    conf.getLastMove match {
      case None => Unit
      case default => fail("The last move should have been None")
    }

    def assertEquals(expected: (Position, Position), actual: Option[(Position, Position)]) = {
      actual match {
        case Some((_, _)) => Unit
        case default => fail("The last move should have been Some((_, _)) but was " + conf.getLastMove)
      }
      val (e1, e2) = expected
      val Some((start, end)) = actual
      assert(start == e1)
      assert(end == e2)

    }
    // TODO: Stop repeating position strings
    conf.move("e2", "e3")
    assertEquals(("e2", "e3"), conf.getLastMove)

    conf.move("h8", "h1")
    assertEquals(("h8", "h1"), conf.getLastMove)
  }

  def moveHistoryCopied = fail

}