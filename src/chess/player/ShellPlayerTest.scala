package chess.player
import chess.model.Colours._
import chess.model._
import chess.model.Configuration
import chess.model.GridConfiguration
import chess.model.MoveExplorer
import chess.model.StandardMoveExplorer
import chess.util.TODO
import test.Main
import test.Test
import test.TestUtils

// TODO: Add shell player that can be configured with a filter and a move grouper
// TODO: Mixin a piece value source to influence capturing player

object ShellPlayerTest extends Test with TestUtils with Main {

  def runTests {
    usesGroupingFunction
  }

  private def usesGroupingFunction {

    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook())
    conf.add("b1", White, Knight())
    conf.add("c1", White, Bishop())
    val mr = new Object with MoveRanker {
      def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
        val knightMoves = moves filter {
          case move: SimpleMove => {
            val (_, piece, _) = conf.getExistingPiece(move.start)
            piece == Knight()
          }
          case default => false
        }
        assert(knightMoves.length > 0)
        // TODO: Find a way to subtract one  list from another
        val otherMoves = moves filterNot { move => knightMoves.contains(move) }
        List(knightMoves, otherMoves)
      }
    }
    val sp = newShellPlayer(conf, mr)
    val Some(move: MovePiece) = sp.getMove(conf.copyOf)

    assertIsInstanceOf(classOf[MovePiece], move, "A move was selected")
    val (colour, piece, _) = conf.getExistingPiece(move.start)
    assertEquals(White, colour, "A white piece was moved")
    assertEquals(Knight(), piece, "A knight was moved")
  }

  private def newShellPlayer(conf: Configuration, moveRanker: MoveRanker): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new ShellPlayer(White, explorer, moveRanker)
  }

  private def addWhiteKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King())
  }

}