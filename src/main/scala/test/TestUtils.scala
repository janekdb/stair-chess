package test

import chess.model.{ Move, Piece, Position, MovePiece }
import chess.model.Configuration
import chess.model.Colours.{Black,White}
import chess.model.King

/** Chess specific helpers to simplify tests. */
trait TestUtils {

  implicit def piece2List(t: Piece) = List(t)
  implicit def string2MovePieceOption(s: String) = Some(new MovePiece(s))
  implicit def string2MovePiece(s: String) = new MovePiece(s)
  implicit def moveMoveOption(m: Move) = Some(m)
  implicit def string2Position(s: String) = new Position(s)

  protected def addWhiteKing(conf: Configuration): Unit = {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King)
  }

  protected def addBlackKing(conf: Configuration): Unit = {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e8", Black, King)
  }

  protected def addKings(conf: Configuration): Unit = {
    for(op <- List(addWhiteKing _, addBlackKing _)) op(conf)
  }
}