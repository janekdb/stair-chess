package chess.ranker
import test.TestUtils
import test.Test
import test.Main
import chess.model.GridConfiguration
import chess.model.Move
import chess.model.StandardMoveExplorer
import chess.model.ConfigurationView
import chess.model.Configuration
import chess.model.Colours.{ Black, White }
import chess.model.Rook
import chess.model.Knight
import chess.model.Bishop
import chess.model.MovePieceCapturing

object CapturingRankerTest extends Test with TestUtils with Main {

  def runTests: Unit = {
    capturingIsRankedFirst
  }

  private def capturingIsRankedFirst: Unit = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CapturingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addKings(conf)
    conf.add("h1", White, Rook)
    conf.add("f8", White, Bishop)
    conf.add("g8", White, Knight)
    conf.add("h8", Black, Rook)

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    assertEquals(2, rankedMoves.size)
    val r = rankedMoves.head
    assertEquals(List(new MovePieceCapturing("h1h8")), r)
  }

}
