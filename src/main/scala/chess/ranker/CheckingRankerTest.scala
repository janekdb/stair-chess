package chess.ranker
import test.TestUtils
import test.Test
import test.Main
import chess.model.GridConfiguration
import chess.model.MovePiece
import chess.model.Configuration
import chess.model.Colours.White
import chess.model.Knight
import chess.model.Pawn
import chess.player.Player
import chess.model.Move
import chess.model.MovePieceCapturing
import chess.model.StandardMoveExplorer
import chess.model.ConfigurationView

object CheckingRankerTest extends Test with TestUtils with Main {

  def runTests: Unit = {
    alwaysChecks
  }

  private def alwaysChecks: Unit = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CheckingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("c4", White, Knight)
    conf.add("g5", White, Pawn)
    val expectedKnightMove = new MovePiece("c4d6")

    if (false) {
      val rp: Player = null
      assertEquals(Some(expectedKnightMove), rp.getMove(conf.copyOf), "A checking move should have been picked")
      /* Do not capture the king */
      conf.applyMove(new MovePiece("c4b2"))
      /* Move the black king to a location where the white pawn can check it on the next move */
      conf.applyMove(new MovePiece("e8f7"))
      val expectedPawnMove = new MovePiece("g5g6")
      assertEquals(Some(expectedPawnMove), rp.getMove(conf.copyOf), "A checking move should have been picked")

    }
    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    /* Visual inspection of the ranked moves confirmed the partition count. */
    assertEquals(2, rankedMoves.size, rankedMoves.toString)
    assertEquals(List(expectedKnightMove), rankedMoves(0))
    assertTrue(rankedMoves(1) contains new MovePiece("g5g6"), rankedMoves(1).toString)
  }
}