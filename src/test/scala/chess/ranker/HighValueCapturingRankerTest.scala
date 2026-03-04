package chess.ranker

import chess.model.Colour.{Black, White}
import chess.model.*
import org.scalatest.*
import wordspec.AnyWordSpec
import matchers.should.Matchers
import chess.test.TestUtils

//noinspection ZeroIndexToHead
class HighValueCapturingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A HighValueCapturingRanker" when {
    "ranking" should {
      "rank high value capturing moves first" in {
        ranksHighValuePiecesFirst
        ranksHighValuePiecesFirstEnPassant
        ranksHighValuePiecesFirstPromoteCapturing
      }
    }
  }

  private val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
  private val ranker = new HighValueCapturingRanker(explorerFactory, White)

  private def ranksHighValuePiecesFirst: Assertion = {

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("a2", White, Rook)
    conf.add("b2", White, Rook)
    conf.add("c2", White, Rook)
    conf.add("a7", Black, Pawn)
    conf.add("b7", Black, Knight)
    conf.add("c7", Black, Queen)

    val explorer                      = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 4
    rankedMoves(0) shouldBe MovePieceCapturing("c2", "c7") :: Nil
    rankedMoves(1) shouldBe MovePieceCapturing("b2", "b7") :: Nil
    rankedMoves(2) shouldBe MovePieceCapturing("a2", "a7") :: Nil

    rankedMoves(3) should not be empty
  }

  private def ranksHighValuePiecesFirstEnPassant: Assertion = {

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("b7", Black, Pawn)
    conf.add("d6", Black, Rook)
    conf.add("c5", White, Pawn)

    conf.applyMove(MovePiece("b7", "b5"))

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 3
    rankedMoves(0) shouldBe MovePieceCapturing("c5", "d6") :: Nil
    rankedMoves(1) shouldBe EnPassant("c5", "b6") :: Nil
    rankedMoves(2) should not be empty
  }

  private def ranksHighValuePiecesFirstPromoteCapturing: Assertion = {
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("f8", Black, Queen)
    conf.add("h8", Black, Rook)
    conf.add("g7", White, Pawn)


    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 3
    rankedMoves(0) shouldBe PromoteCapturing("g7", "f8", Knight) :: PromoteCapturing("g7", "f8", Queen) :: Nil
    rankedMoves(1) shouldBe PromoteCapturing("g7", "h8", Knight) :: PromoteCapturing("g7", "h8", Queen) :: Nil
    rankedMoves(2) should not be empty
  }
}
