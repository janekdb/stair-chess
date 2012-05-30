package chess.player

import chess.model.{ Castle, EnPassant, Move, MovePiece, MovePieceCapturing, Promote, PromoteCapturing }
import chess.model.{ Configuration, MoveExplorer }
import chess.model.Colour
import chess.model.Position
import chess.model.Pawn
import chess.model.Constants
import chess.model.{Knight, Queen}
import chess.model.{ Long, Short }

// TODO: Move move generation to StandardMoveExplorer
// TODO: Rework checkmate escape code to use list of valid moves

/**
 * A player that selects moves at random.
 */
class RandomPlayer(val colour: Colour, val conf: Configuration, val explorer: MoveExplorer) extends Player {

  def getMove: Move = {
    val startPositions = conf.locatePieces(colour)
    def isHomeRow(row: Int): Boolean = row == Constants.WHITE_HOME_ROW || row == Constants.BLACK_HOME_ROW
    def isDiagonal(a: Position, b: Position): Boolean = a.col != b.col
    var moves = List[Move]()
    val ps = List(Knight(), Queen())
    for (s <- startPositions) {
      var endPositions = explorer.getBasicPositions(s)
      val (_, piece, _) = conf.getExistingPiece(s)
      // TODO: Switch to functional approach with yield
      endPositions.foreach { end =>
        val endOccupied = conf.getPiece(end).isDefined
        val ms = piece match {
          case Pawn() if isHomeRow(end.getRow) =>
            if (endOccupied) ps.map {PromoteCapturing(s, end, _)} else ps.map {Promote(s, end, _)}
          case Pawn() if (!endOccupied && isDiagonal(s, end)) => List(EnPassant(s, end))
          case default => if (endOccupied) List(MovePieceCapturing(s, end)) else List(MovePiece(s, end))
        }
        moves = ms ::: moves
      }
    }

    /*
     * Only add castling if the king and rooks are at the correct positions because
     * rejectIllegalMove does not explicitly check the pieces are present.
     */
    for (castlingType <- List(Long, Short)) {
      val ((king, _), (rook, _)) = castlingType.getPositions(colour.homeRow)
      if (List(king, rook).forall(conf.exists(_, colour))) {
        moves = Castle(colour, castlingType) :: moves
      }
    }
    moves = moves filter { moveAcceptable }
    if (moves.isEmpty) throw new RuntimeException("No move found")
    moves(new util.Random().nextInt(moves.size))
  }

  protected def moveAcceptable(move: Move): Boolean = {
    try {
      // TODO: Convert rejectIllegalMove to a query method
      explorer.rejectIllegalMove(move)
      true
    } catch {
      case e => false
    }
  }

  // TODO: Improve shuffling, maybe use Fisher-Yates shuffling algorithm.
  //  private def shuffle(a: Array[Position]) = {
  //    java.util.Collections.shuffle(java.util.Arrays.asList(a: _*))
  //  }
}