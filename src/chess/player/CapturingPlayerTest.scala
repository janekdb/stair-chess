package chess.player

import test.Main
import test.Test
import test.TestUtils
import chess.model.MoveExplorer
import chess.model.StandardMoveExplorer
import chess.model.Configuration
import chess.model.Colours._
import chess.model.GridConfiguration
import chess.model.King
import chess.model.Rook
import chess.model.Pawn
import chess.model.MovePieceCapturing

object CapturingPlayerTest extends Test with TestUtils with Main {

  def runTests {
    attackingMovesPreferred
  }

  // TODO: Devise a way to confirm capturing moves are explicitly selected.
  private def attackingMovesPreferred {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook())
    conf.add("a2", Black, Pawn())
    val rp = newPlayer(conf)
    val move = rp.getMove
    assertNotNull(move, "A move should be available")
    assertEquals(new MovePieceCapturing("a1", "a2"), move, "The capturing moves should have been selected")
  }

  private def newPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new CapturingPlayer(White, explorer)
  }

  private def addWhiteKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King())
  }

}