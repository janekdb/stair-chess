package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }

class Computer(val board: BoardModel, val colour: Colour) extends Player {

  var moves = List(
    new MovePiece("a2a3"),
    new MovePiece("b2b4"),
    new MovePiece("c2c4"),
    new MovePiece("h2h3"),
    /* Knight */
    new MovePiece("b1c3"),
    /* Knight */
    new MovePiece("g1f3"),
    /* Knight takes pawn */
    new MovePiece("c3d5"),
    new MovePiece("c1b2"),
    /* */
    Resign(colour))

  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }
}