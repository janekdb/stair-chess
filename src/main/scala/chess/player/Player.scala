package chess.player

import chess.model.Move
import chess.model.Configuration

trait Player {
  // TODO: Consider using Either to return Resign or Option[Move]
  def getMove(configuration: Configuration): Option[Move]
  def getName: String
}