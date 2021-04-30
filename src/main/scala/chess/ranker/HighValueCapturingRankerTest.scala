package chess.ranker

import chess.model.Colours.{Black, White}
import chess.model.{Configuration, ConfigurationView, GridConfiguration, Knight, Move, MovePiece, MovePieceCapturing, Pawn, Queen, Rook, StandardMoveExplorer}
import test.{Main, Test, TestUtils}

//noinspection ZeroIndexToHead
object HighValueCapturingRankerTest extends Test with TestUtils with Main {

  def runTests: Unit = {
    ranksHighValuePiecesFirst
  }

  private def ranksHighValuePiecesFirst: Unit = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new HighValueCapturingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("a2", White, Rook)
    conf.add("b2", White, Rook)
    conf.add("c2", White, Rook)
    conf.add("a7", Black, Pawn)
    conf.add("b7", Black, Knight)
    conf.add("c7", Black, Queen)

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    assertEquals(4, rankedMoves.size, rankedMoves.toString)
    assertEquals(new MovePieceCapturing("c2c7") :: Nil, rankedMoves(0))
    assertEquals(new MovePieceCapturing("b2b7") :: Nil, rankedMoves(1))
    assertEquals(new MovePieceCapturing("a2a7") :: Nil, rankedMoves(2))
    assertTrue(rankedMoves(3).nonEmpty)
    //    rankedMoves.foreach(println)
    //    ???
  }
}
