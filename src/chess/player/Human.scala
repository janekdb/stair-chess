package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }

class Human(val board: BoardModel, val colour: Colour) extends Player {

  var moves = List(
    new MovePiece("a7a6"),
    new MovePiece("d7d5"),
    new MovePiece("e7e5"),
    new MovePiece("f7f6"),
    /* Queen */
    new MovePiece("e8f7"),
    /* Rook */
    new MovePiece("a8a7"),
    /* Knight */
    new MovePiece("g8h6"),
    /* Bishop */
    new MovePiece("f8e7"),
    Castle(colour, Long),
    /* Advance Pawn to allow Computer to promote. */
    new MovePiece("a6a5"),
    new MovePiece("a7a6"),
    new MovePiece("a6b6"),
    new MovePiece("g7g6"),
    /* Advance Pawn */
    new MovePiece("f7g7"),
    new MovePiece("g6g5"),
    new MovePiece("g5g4"),
    /* Pawn takes Knight */
    new MovePiece("g4f3"),
    /* Queen */
    new MovePiece("g7h8"),
    new MovePiece("f8f7"),
    new MovePiece("f7f8"),
    new MovePiece("f8f7"),
    new MovePiece("f7f8"),
    new MovePiece("f8f7"),
    new MovePiece("f7f8"),
    // TODO: add some moves to allow white to test re-Castling
    Resign(colour))

  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }
}