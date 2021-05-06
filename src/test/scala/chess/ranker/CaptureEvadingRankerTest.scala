package chess.ranker

import chess.model.Colours.{Black, White}
import chess.model._
import chess.test.TestUtils
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec

class CaptureEvadingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A CaptureEvadingRanker" when {
    "ranking" should {
      "rank capture evading moves first" in {
        attackReducingMovesRankedFirst
      }
      "rank highest impact moves first" in {
        highestImpactAttackReducingMovesRankedFirst
      }
    }
  }

  private def attackReducingMovesRankedFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CaptureEvadingRanker(explorerFactory, White)

    val conf = new GridConfiguration

    addWhiteKing(conf)

    /* A piece that is attacked that is shielding another piece so moving the piece does not reduce the attack count. */
    conf.add("a2", White, Rook)
    conf.add("a1", White, Knight)
    conf.add("a8", Black, Queen)

    /* A piece that can move into a attacked square. */
    conf.add("g1", White, Knight)
    conf.add("h8", Black, Rook)

    /* A piece that can move out of attack. */
    conf.add("e6", White, Pawn)
    conf.add("d5", Black, King)

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    /* Visual inspection of the ranked moves confirmed the partition count. */
    rankedMoves should have size 3
    //noinspection ZeroIndexToHead
    rankedMoves(0) should contain(new MovePiece("e6e7"))
    rankedMoves(1) should contain(new MovePieceCapturing("a2a8"))
    rankedMoves(2) should contain(new MovePiece("g1h3"))
  }

  private def highestImpactAttackReducingMovesRankedFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CaptureEvadingRanker(explorerFactory, White)

    val conf = new GridConfiguration

    addWhiteKing(conf)
    conf.add("a3", Black, King)

    // Add three pieces that are the only pieces attacked. One piece is attacked 3 times, then 2 then 1
    conf.add("d6", White, Pawn)
    conf.add("e6", White, Pawn)
    conf.add("f6", White, Pawn)
    /* This removes double advance moves from consideration. */
    conf.add("a8", Black, Rook)
    /* Attack left and right */
    conf.add("e5", Black, Bishop)
    /* Attack middle */
    conf.add("d4", Black, Knight)
    /* Attack left and right */
    conf.add("e4", Black, Knight)
    /* Attack left */
    conf.add("c4", Black, Knight)

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves.head should equal(new MovePiece("d6d7") :: Nil)
  }
}
