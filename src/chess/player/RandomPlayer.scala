package chess.player

import chess.model.Move
import chess.model.{ Configuration, MoveExplorer }
import chess.model.Colour

// TODO: Replace RandomPlayer with RandomRanker in combination with ShellPlayer

/**
 * A player that selects moves at random.
 */
class RandomPlayer(val colour: Colour, val explorer: MoveExplorer) extends Player {

  def getName = "RandomPlayer"

  def getMove(configuration: Configuration): Option[Move] = {
    val moves = explorer.legalMoves(colour)
    if (moves.isEmpty) {
      None
    } else {
      Some(moves(new util.Random().nextInt(moves.size)))
    }
  }
}