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
    Castle(colour, Short),
    new MovePiece("a3a4"),
    new MovePiece("b4a5"),
    new MovePiece("b2c3"),
    new MovePiece("a5a6"),
    new MovePiece("a6a7"),
    new Promote("a7a8", Queen()),
    new MovePiece("a4a5"),
    new MovePiece("a5a6"),
    new MovePiece("a6a7"),
    new Promote("a7b8", Queen()),
    /* Move King and Rook back to initial positions to check castling after moving is prevented. */
    new MovePiece("c1c2"),
    new MovePiece("c2a2"),
    new MovePiece("a2a1"),
    new MovePiece("b1c1"),
    new MovePiece("c1d1"),
    Castle(colour, Short),
    /* */
    Resign(colour))

  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }
}