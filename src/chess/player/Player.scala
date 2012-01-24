package chess.player

import chess.model.Move

trait Player {
  def getMove: Move
}