package chess.player

import chess.model.{ Move, MovePiece }
import chess.model.{ Configuration, MoveExplorer }
import chess.model.Colour
import chess.model.Position

// TODO: Ensure all move types can be generated
// TODO: Ensure en-passant moves are generated by StandardMoveExplorer
// TODO: Ensure promotion moves can be generated
// TODO: Ensure either castling type can be generated

/**
 * A player that selects moves at random.
 */
class RandomPlayer(val colour: Colour, val conf: Configuration, val explorer: MoveExplorer) extends Player {

  def getMove: Move = {
    val startPositions = conf.locatePieces(colour).toArray
    shuffle(startPositions)
    java.util.Collections.shuffle(java.util.Arrays.asList(startPositions: _*))
    for (
      s <- startPositions
    ) {
      var endPositions = explorer.getBasicPositions(s).toArray
      shuffle(endPositions)
      // TODO: Switch to functional approach
      while (endPositions.nonEmpty) {
        val move = MovePiece(s, endPositions.head)
        val moveAccepted =
          try {
            // TODO: Convert rejectIllegalMove to a query method
            explorer.rejectIllegalMove(move)
            true
          } catch {
            // TODO: Try next end position because the current head failed
            case e =>
              { println("rejected move: " + move + ": " + e) }
              false
          }
        if (moveAccepted) return move
        endPositions = endPositions.tail
      }
    }
    throw new RuntimeException("No move found")
  }

  // TODO: Improve shuffling, maybe use Fisher-Yates shuffling algorithm.
  private def shuffle(a: Array[Position]) = {
    java.util.Collections.shuffle(java.util.Arrays.asList(a: _*))
  }
}