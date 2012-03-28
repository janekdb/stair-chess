package chess.model

import chess.util.UnhandledCaseException
import chess.model.ex.{
  AttackedPositionException,
  CheckedOwnKing,
  InterveningPieceException,
  PreviouslyMovedException,
  UnreachablePositionException
}
import Misc.kingInCheck
import chess.util.TODO

/**
 * The moves of standard chess
 */
class StandardMoveExplorer(conf: Configuration) extends MoveExplorer {

  implicit def tuple2list(t: Tuple2[Position, Position]) = List(t._2, t._2)

  /**
   * @return The a set of possible positions excluding moves that would result in 1. the move escaping from the board edges,
   * or 2. A non-Knight jumping over a piece, or 3. A piece taking another piece of the same colour
   */
  def getBasicPositions(position: Position): Set[Position] = {
    val (colour, piece, _) = conf.getExistingPiece(position)

    val vectors = piece.movements(colour)
    // TODO: Move repeatable property into vectors by pre-repeating the vectors
    val repeatable = piece match {
      case Queen() => true
      case Bishop() => true
      case Rook() => true
      case default => false
    }
    var basicPositions = Set[Position]()

    val moveAllowed = piece match {
      case Pawn() => pawnMoveAllowed _
      case default => anyMoveAllowed _
    }

    for (v <- vectors) {
      var advanceTerminated = false
      var firstIteration = true
      var c = position
      while ((firstIteration || repeatable) && (!advanceTerminated) && c.canOffset(v._1, v._2)) {
        firstIteration = false
        if (!moveAllowed(c, v)) {
          advanceTerminated = true
        } else {
          c = c.offset(v._1, v._2)
          val (ownPiece, opponentPiece) = testPieceColour(c, colour)
          /* Empty square or opponents piece */
          if (!ownPiece) basicPositions = basicPositions + c
          if (ownPiece || opponentPiece) advanceTerminated = true
        }
      }
    }
    basicPositions
  }

  private def anyMoveAllowed(startPosition: Position, d: (Int, Int)) = true

  private def pawnMoveAllowed(startPosition: Position, d: (Int, Int)) = {
    val (dCol, dRow) = d
    if (pawnDiagonal(dCol, dRow)) {
      /* Can only take if piece present or en passant possible. The client code will check for the colour */
      val p = startPosition.offset(dCol, dRow)
      val lastMoveWasDoublePawn = conf.getLastMove match {
        case Some((Pawn(), start, end)) if (start.getRow - end.getRow).abs == 2 => true
        case default => false
      }
      var lastMoveWasAdjacentColumn = conf.getLastMove match {
        case Some((Pawn(), start, end)) if end.getCol == p.getCol => true
        case default => false
      }
      var rowIsEnPassant = {
        val (colour, _, _) = conf.getExistingPiece(startPosition)
        startPosition.getRow == colour.enPassantRow
      }
      conf.exists(p) || (lastMoveWasDoublePawn && lastMoveWasAdjacentColumn && rowIsEnPassant)
    } else if (pawnForward(dCol, dRow)) {
      /* Prevent forward capturing. */
      val p = startPosition.offset(dCol, dRow)
      !conf.exists(p)
    } else if (pawnForwardTwo(dCol, dRow)) {
      conf.getExistingPiece(startPosition) match {
        /* Disallow double advancement if the pawn has already been moved. */
        case (_, _, Some(_)) => false
        case default => {
          /* Deny attempt to jump over a piece */
          val r = if (dRow == 2) 1 else -1
          val p = startPosition.offset(0, r)
          !conf.exists(p)
        }
      }
    } else {
      throw new AssertionError("All pawn moves handled")
    }
  }

  // TODO: Switch to contains test on list of moves with the complement added via map
  private def pawnDiagonal(dCol: Int, dRow: Int): Boolean = (dCol == 1 | dCol == -1) & (dRow == 1 | dRow == -1)
  private def pawnForward(dCol: Int, dRow: Int): Boolean = dCol == 0 & (dRow == 1 | dRow == -1)
  private def pawnForwardTwo(dCol: Int, dRow: Int): Boolean = dCol == 0 & (dRow == 2 | dRow == -2)

  /** @return (encountered own piece, encountered opponent piece) */
  def testPieceColour(movePiecePosition: Position, movingPieceColour: Colour) = {
    conf.getPiece(movePiecePosition) match {
      case None => { /* Square unoccupied */ (false, false) }
      case Some((otherColour, _, _)) => {
        if (otherColour == movingPieceColour) (true, false) else (false, true)
      }
    }
  }

  def rejectIllegalMove(move: Move) {
    def checkReachable(start: Position, end: Position) = {
      val legalPositions = getBasicPositions(start)
      if (!(legalPositions contains end)) {
        throw new UnreachablePositionException(MovePiece(start, end), legalPositions)
      }
    }

    move match {
      case MovePiece(start, end) => {
        checkReachable(start, end)
        checkKingNotLeftInCheckAfterMove(MovePiece(start, end))
      }
      case Castle(colour, castlingType) => {
        /*
        * Castling restrictions.
        * 1. Your king has been moved earlier in the game.
		* 2. The rook that castles has been moved earlier in the game.
		* 3. There are pieces standing between your king and rook.
		* 4. The king is in check.
		* 5. The king moves through a square that is under attack by an opponents piece.
		* 6. The king would be in check after castling.
        */

        val row = colour.homeRow

        val ((king, kingEnd), (rook, _)) = castlingType.getPositions(row)

        /* Disallow if either piece has already been moved. */
        (king, rook).foreach { p =>
          conf.getExistingPiece(p) match {
            case (_, _, Some(_)) => throw new PreviouslyMovedException(move)
            case default => Unit
          }
        }
        /* Disallow if there are any pieces between the rook and king */
        val interveningPositions = Position.getInterveningPositions(king, rook)
        interveningPositions.foreach { p =>
          conf.getPiece(p) match {
            case Some(_) => throw new InterveningPieceException(move, p)
            case default => Unit
          }
        }

        /* Disallow if King is in check or would cross any square that is is attacked or would end in check. */
        // TODO: Consider converting to map operation with predicate to test for attacked status
        val exposedPositions = king :: kingEnd :: Position.getInterveningPositions(king, kingEnd) toSet
        val opponentPositions = conf.locatePieces(colour.opposite)
        opponentPositions.foreach { p =>
          val attackedPositions = getBasicPositions(p)
          val i = exposedPositions.intersect(attackedPositions)
          if (i.nonEmpty) {
            throw new AttackedPositionException(move, i.head)
          }
        }

      }
      case Promote(start, end, piece) => {
        checkReachable(start, end)
        /* For this purpose Promote is the same as MovePiece */
        checkKingNotLeftInCheckAfterMove(MovePiece(start, end))
      }
      case EnPassant(start, end) => {
        checkReachable(start, end)
        checkKingNotLeftInCheckAfterMove(EnPassant(start, end))
      }
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

  private def checkKingNotLeftInCheckAfterMove(move: SimpleMove) {
    /*
	   * 1. Clone the current conf
	   * 2. Apply the move without recursively calling this method
	   * 3. See if the King is in check
	   */
    val (colour, _, _) = conf.getExistingPiece(move.start)
    val future = conf.copyOf
    future.applyMove(move)

    if (kingInCheck(colour, future)) {
      throw new CheckedOwnKing(move)
    }    
  }
  
  private def log(message: String) = {
    println(message);
  }
}