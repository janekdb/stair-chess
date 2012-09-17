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
    conf.add("g5", White, Pawn())
    val rp = newPlayer(conf)
    val expectedKnightMove = new MovePiece("c4d6")
    assertEquals(Some(expectedKnightMove), rp.getMove(conf.copyOf), "A checking move should have been picked")
    /* Do not capture the king */
    conf.applyMove(new MovePiece("c4b2"))
    /* Move the black king to a location where the white pawn can check it on the next move */
    conf.applyMove(new MovePiece("e8f7"))
    val expectedPawnMove = new MovePiece("g5g6")
    assertEquals(Some(expectedPawnMove), rp.getMove(conf.copyOf), "A checking move should have been picked")
  }

  private def newPlayer(conf: Configuration): Player = {
    val explorerFactory = (conf: Configuration) => new StandardMoveExplorer(conf)
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new CheckingPlayer(White, explorer, explorerFactory)
  }

}