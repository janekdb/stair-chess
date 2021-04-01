package chess.ranker
import test.TestUtils
import test.Test
import test.Main
import chess.model.Move
import chess.model.ConfigurationView
import chess.model.GridConfiguration
import chess.model.Configuration
import chess.model.Colours.{ Black, White }
import chess.model.Knight
import chess.model.Bishop
import chess.model.Rook
import chess.model.King
import scala.collection.immutable.StringOps
import chess.model.Pawn
import chess.model.StandardMoveExplorer
import chess.model.MovePiece
import chess.model.MoveExplorer
import chess.model.Colour

object CheckMatingRankerTest extends Test with TestUtils with Main {

  def runTests {
    checkMateIsRankedFirst
    whenNoCheckMateAllMovesAreRankedEqually
  }

  private def checkMateIsRankedFirst {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CheckMatingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addWhiteKing(conf)
    conf.add("h1", White, Rook)

    /* Trap the king behind a row of pawns */
    conf.add("a8", Black, King)
    List('a', 'b', 'c').foreach { c => conf.add(c + "7", Black, Pawn) }

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    assertEquals(2, rankedMoves.size)
    val r = rankedMoves.head
    assertEquals(List(new MovePiece("h1h8")), r)
  }

  private def whenNoCheckMateAllMovesAreRankedEqually {
	  val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
	  val ranker = new CheckMatingRanker(explorerFactory, White)
	  
	  val conf: Configuration = new GridConfiguration
	  
	  addKings(conf)
	  conf.add("h1", White, Rook)
	  
	  val explorer = new StandardMoveExplorer(conf)
	  val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)
	  
	  assertEquals(1, rankedMoves.size)
  }
  
}