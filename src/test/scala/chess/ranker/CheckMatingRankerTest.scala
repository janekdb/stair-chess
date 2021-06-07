package chess.ranker

import chess.model.Colours.{Black, White}
import chess.model._
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec
import chess.test.TestUtils

class CheckMatingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A CheckMatingRanker" when {
    "ranking" should {
      "rank checkmating moves first" in {
        checkMateIsRankedFirst
      }
      "rank all moves equally if no checkmating moves present" in {
        whenNoCheckMateAllMovesAreRankedEqually
      }
    }
  }

  private def checkMateIsRankedFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker          = new CheckMatingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addWhiteKing(conf)
    conf.add("h1", White, Rook)

    /* Trap the king behind a row of pawns */
    conf.add("a8", Black, King)
    List('a', 'b', 'c').foreach { c => conf.add(s"${c}7", Black, Pawn) }

    val explorer                      = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 2
    rankedMoves.head shouldBe new MovePiece("h1h8") :: Nil
  }

  private def whenNoCheckMateAllMovesAreRankedEqually: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker          = new CheckMatingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addKings(conf)
    conf.add("h1", White, Rook)

    val explorer                      = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 1
  }

}
