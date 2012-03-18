package test

import chess.model.{Piece, Position, MovePiece}

/** Chess specifc helpers to simplify tests. */
trait TestUtils {

  implicit def piece2List(t: Piece) = List(t)
  implicit def string2MovePiece(s: String) = new MovePiece(s)
  implicit def string2Position(s: String) = new Position(s)

}