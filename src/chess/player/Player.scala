package chess.player

import chess.model.Move
import chess.model.Configuration

trait Player {
  def getMove(configuration: Configuration): Option[Move]
  def getName: String
}