package chess.player

import chess.model.Move
import chess.model.{ Configuration, MoveExplorer }
import chess.model.Colour
import chess.model.StandardMoveExplorer

/**
 * A player that will always check if possible
 */
class CheckingPlayer(val colour: Colour, val explorer: MoveExplorer, val explorerFactory: Configuration => MoveExplorer) extends Player {

  def getMove(configuration: Configuration): Option[Move] = {
    val moves = explorer.legalMoves(colour)
    if (moves.isEmpty) {
      None
    } else {
      val cMoves: List[Move] = checkingMoves(configuration, moves)
      val ms = if (cMoves.size > 0) cMoves else moves
      Some(ms(new util.Random().nextInt(ms.size)))
    }
  }

  private def checkingMoves(configuration: Configuration, moves: List[Move]): List[Move] = {
    moves filter { m =>
      val conf = configuration.copyOf
      conf.applyMove(m)
      val e = explorerFactory(conf)
      e.kingInCheck(colour.opposite)
    }
  }
}