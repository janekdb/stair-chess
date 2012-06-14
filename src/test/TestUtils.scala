package test

import chess.model.{Move, Piece, Position, MovePiece}

/** Chess specifc helpers to simplify tests. */
trait TestUtils {

  implicit def piece2List(t: Piece) = List(t)
  implicit def string2MovePieceOption(s: String) = Some(new MovePiece(s))
  implicit def string2MovePiece(s: String) = new MovePiece(s)
  implicit def moveMoveOption(m: Move) = Some(m)
  implicit def string2Position(s: String) = new Position(s)

}