package chess.player

import test.{ Main, Test, TestUtils }
import chess.model._
import chess.model.Colours._

object RandomPlayerTest extends Test with TestUtils with Main {

  def runTests {
    canMove
    isRandom
    selectsOnlyMove
  }

  private def canMove {
    val conf: Configuration = new GridConfiguration
    addKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    assertNotNull(rp.getMove, "A move should be available")
  }

  private def isRandom {
    val conf: Configuration = new GridConfiguration
    addKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    val m1 = rp.getMove
    conf.applyMove(m1)
    val m2 = rp.getMove
    assertNotEquals(m1, m2, "The second move should be different to the first")
  }

  private def selectsOnlyMove {
    val conf: Configuration = new GridConfiguration
    addKing(conf)
    /* Box the rooks in */
    conf.add("a6", White, Pawn());
    conf.add("a7", White, Rook());
    conf.add("b7", White, Pawn());
    conf.add("a8", White, Rook());
    conf.add("b8", Black, Rook());
    val rp = newRandomPlayer(conf)
    val m = rp.getMove
    assertEquals(MovePiece("a8", "b8"), m, "The only possible move should have been selected")
  }

  private def newRandomPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(White, conf, explorer)
  }

  private def addKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King())
  }
}