package chess.ranker
import test.TestUtils
import test.Test
import test.Main
import chess.model.GridConfiguration
import chess.model.MovePiece
import chess.model.Move
import chess.model.StandardMoveExplorer
import chess.model.Configuration
import chess.model.Colours.{ Black, White }
import chess.model.Pawn
import chess.model.King
import chess.model.Rook
import chess.model.Knight
import chess.model.Queen
import chess.model.ConfigurationView
import chess.model.MovePieceCapturing
import chess.model.Bishop

object CaptureEvadingRankerTest extends Test with TestUtils with Main {

  def runTests {
    attackReducingMovesRankedFirst
    highestImpactAttackReducingMovesRankedFirst
  }

  private def attackReducingMovesRankedFirst {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CaptureEvadingRanker(explorerFactory, White)

    val conf = new GridConfiguration

    addWhiteKing(conf)

    /* A piece that is attacked that is shielding another piece so moving the piece does not reduce the attack count. */
    conf.add("a2", White, Rook())
    conf.add("a1", White, Knight())
    conf.add("a8", Black, Queen());

    /* A piece that can move into a attacked square. */
    conf.add("g1", White, Knight())
    conf.add("h8", Black, Rook())

    /* A piece that can move out of attack. */
    conf.add("e6", White, Pawn())
    conf.add("d5", Black, King())

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    /* Visual inspection of the ranked moves confirmed the partition count. */
    assertEquals(3, rankedMoves.size, rankedMoves.toString)
    assertTrue(rankedMoves(0) contains new MovePiece("e6e7"), rankedMoves(0).toString)
    assertTrue(rankedMoves(1) contains new MovePieceCapturing("a2a8"), rankedMoves(1).toString)
    assertTrue(rankedMoves(2) contains new MovePiece("g1h3"), rankedMoves(2).toString)
  }

  private def highestImpactAttackReducingMovesRankedFirst {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CaptureEvadingRanker(explorerFactory, White)

    val conf = new GridConfiguration

    addWhiteKing(conf)
    conf.add("a3", Black, King())

    // Add three pieces that are the only pieces attacked. One piece is attacked 3 time, then 2 then 1
    conf.add("d6", White, Pawn())
    conf.add("e6", White, Pawn())
    conf.add("f6", White, Pawn())
    /* This removes double advance moves from consideration. */
    conf.add("a8", Black, Rook())
    /* Attack left and right */
    conf.add("e5", Black, Bishop())
    /* Attack middle */
    conf.add("d4", Black, Knight())
    /* Attack left and right */
    conf.add("e4", Black, Knight())
    /* Attack left */
    conf.add("c4", Black, Knight())

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    assertEquals(List(new MovePiece("d6d7")), rankedMoves.head)
  }

  private def render(conf: ConfigurationView) {
    val lines = ConfigurationView.getTextRepresentation(conf)
    for (line <- lines) println(line)
    println
  }

}