package chess.test

import chess.model.{Move, Piece, Position, MovePiece}
import chess.model.Configuration
import chess.model.Colour.{Black, White}
import chess.model.King

/** Chess specific helpers to simplify tests. */
trait TestUtils {

  given Conversion[Piece, List[Piece]] = List(_)

  given Conversion[String, Some[MovePiece]] = t => Some(new MovePiece(t))

  given Conversion[String, MovePiece] = new MovePiece(_)

  given Conversion[Move, Some[Move]] = Some(_)

  given Conversion[String, Position] = new Position(_)

  protected def addWhiteKing(conf: Configuration): Unit = {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King)
  }

  protected def addBlackKing(conf: Configuration): Unit = {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e8", Black, King)
  }

  protected def addKings(conf: Configuration): Unit = {
    for (op <- List(addWhiteKing, addBlackKing)) op(conf)
  }
}
