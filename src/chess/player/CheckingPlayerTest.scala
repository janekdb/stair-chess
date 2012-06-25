package chess.player

import test.{ Main, Test, TestUtils }
import chess.model._
import chess.model.Colours._

object CheckingPlayerTest extends Test with TestUtils with Main {

  implicit def optMove2Move(optMove: Option[Move]) = optMove.get

  def runTests {
    canMove
    alwaysChecks
  }

  private def canMove {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    addBlackKing(conf)
    val rp = newPlayer(conf)
    assertNotNull(rp.getMove(conf.copyOf), "A move should be available")
  }

  private def alwaysChecks {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    addBlackKing(conf)
    conf.add("c4", White, Knight())
    // TODO: Add pawn to check next
    val rp = newPlayer(conf)
    val expectedKnightMove = new MovePiece("c4d6")
    assertEquals(Some(expectedKnightMove), rp.getMove(conf.copyOf), "A checking move should have been picked")
    conf.applyMove(expectedKnightMove)
  }

  private def newPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new CheckingPlayer(White, explorer)
  }

  private def addWhiteKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King())
  }

  private def addBlackKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e8", Black, King())
  }

}