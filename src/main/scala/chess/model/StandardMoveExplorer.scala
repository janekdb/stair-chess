package chess.model

import chess.util.UnhandledCaseException
import chess.model.ex.{
  AttackedPositionException,
  CapturingMoveException,
  CheckedOwnKing,
  IllegalMoveException,
  InterveningPieceException,
  InvalidParticipantException,
  NonCapturingMoveException,
  PreviouslyMovedException,
  UnreachablePositionException
}
import chess.util.TODO
import chess.model.ex.NonPromotingPawnAdvance
import scala.language.postfixOps

/**
 * The moves of standard chess
 */
class StandardMoveExplorer(conf: ConfigurationView) extends MoveExplorer {

  private implicit def tuple2list(t: (Position, Position)): List[Position] = List(t._1, t._2)

  /**
   * The set of attacked positions including empty squares that would be attacked by a pawn
   * if an opposition piece were present
   */
  private def getAttackedPositions(position: Position): Set[Position] = {
    getEndPositions(position, pawnAttackingMoveAllowed)
  }

  def getBasicPositions(position: Position): Set[Position] = {
    getEndPositions(position, pawnMoveAllowed)
  }

  /**
   * @return The set of possible positions excluding moves that would result in 1. the move escaping from the board edges,
   * or 2. A non-Knight jumping over a piece, or 3. A piece taking another piece of the same colour.
   * Further restrictions on moves are imposed by {@link #rejectIllegalMove}
   */
  def getEndPositions(position: Position,
    pawnMovePredicate: (Position, (Int, Int)) => Boolean): Set[Position] = {
    val (colour, piece, _) = conf.getExistingPiece(position)

    val vectors = piece.movements(colour)
    // TODO: Move repeatable property into vectors by pre-repeating the vectors
    val repeatable = piece match {
      case Queen => true
      case Bishop => true
      case Rook => true
      case default => false
    }
    var basicPositions = Set[Position]()

    val moveAllowed = piece match {
      case Pawn => pawnMovePredicate
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

  private def pawnMoveAllowed(startPosition: Position, vector: (Int, Int)) = {
    val (dCol, dRow) = vector
    if (pawnDiagonal(dCol, dRow)) {
      /* Can only take if piece present or en passant possible. The client code will check for the colour */
      val p = startPosition.offset(dCol, dRow)
      val lastMoveWasDoublePawn = conf.getLastMove match {
        case Some((Pawn, start, end)) if (start.getRow - end.getRow).abs == 2 => true
        case default => false
      }
      val lastMoveWasAdjacentColumn = conf.getLastMove match {
        case Some((Pawn, start, end)) if end.getCol == p.getCol => true
        case default => false
      }
      val rowIsEnPassant = {
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
        case default =>
          /* Deny attempt to jump over a piece */
          val r = if (dRow == 2) 1 else -1
          val p = startPosition.offset(0, r)
          !conf.exists(p)
      }
    } else {
      throw new AssertionError("All pawn moves handled")
    }
  }

  /* Allow pawn attacking moves as normal but also allow attacks on empty squares. Do not include
   * forward moves because these are not attacking moves. */
  private def pawnAttackingMoveAllowed(startPosition: Position, vector: (Int, Int)) = {

    val (dCol, dRow) = vector
    if (pawnDiagonal(dCol, dRow)) {
      true
    } else if (pawnForward(dCol, dRow)) {
      false
    } else if (pawnForwardTwo(dCol, dRow)) {
      false
    } else {
      throw new AssertionError("All pawn moves handled")
    }
  }

  private def pawnDiagonal(dCol: Int, dRow: Int) = Set((1, 1), (1, -1), (-1, -1), (-1, 1)) contains (dCol, dRow)
  private def pawnForward(dCol: Int, dRow: Int) = Set((0, 1), (0, -1)) contains (dCol, dRow)
  private def pawnForwardTwo(dCol: Int, dRow: Int) = Set((0, 2), (0, -2)) contains (dCol, dRow)

  /** @return (encountered own piece, encountered opponent piece) */
  def testPieceColour(movePiecePosition: Position, movingPieceColour: Colour): (Boolean, Boolean) = {
    conf.getPiece(movePiecePosition) match {
      case None => /* Square unoccupied */ (false, false)
      case Some((otherColour, _, _)) =>
        if (otherColour == movingPieceColour) (true, false) else (false, true)
    }
  }

  def rejectIllegalMove(move: Move): Unit = {

    def checkReachable(start: Position, end: Position): Unit = {
      val legalPositions = getBasicPositions(start)
      if (!(legalPositions contains end)) {
        throw new UnreachablePositionException(MovePiece(start, end), legalPositions)
      }
    }

    def checkNotNonPromotingPawnAdvance(start: Position, end: Position): Unit = {
      val (_, piece, _) = conf.getExistingPiece(start)
      piece match {
        case Pawn =>
          if (end.getRow == Constants.WHITE_HOME_ROW || end.getRow == Constants.BLACK_HOME_ROW)
            throw new NonPromotingPawnAdvance(move)
        case default => ()
      }
    }

    def checkNotCapturing(end: Position): Unit = {
      if (conf.getPiece(end).isDefined) {
        throw new NonCapturingMoveException(move)
      }
    }

    def checkCapturing(end: Position): Unit = {
      if (conf.getPiece(end).isEmpty) {
        throw new CapturingMoveException(move)
      }
    }

    move match {
      case m @ MovePiece(start, end) =>
        checkReachable(start, end)
        /* If the move was a promotion it would be matched by Promote */
        checkNotNonPromotingPawnAdvance(start, end)
        /* MovePieceCapturing must be used when capturing */
        checkNotCapturing(end)
        /* Perform this check after all move validating checks */
        checkKingNotLeftInCheckAfterMove(m)
      case m @ MovePieceCapturing(start, end) =>
        checkReachable(start, end)
        /* If the move was a promotion it would be matched by Promote */
        checkNotNonPromotingPawnAdvance(start, end)
        /* MovePiece must be used when not capturing */
        checkCapturing(end)
        /* Perform this check after all move validating checks */
        checkKingNotLeftInCheckAfterMove(m)
      case Castle(colour, castlingType) =>
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

        /* Disallow if the pieces are not a rook and king in the correct positions */
        List((rook, Rook), (king, King)).foreach {
          case (p, t) =>
            val (_, piece, _) = conf.getExistingPiece(p)
            if (piece != t) {
              throw new InvalidParticipantException(move, piece)
            }
        }

        /* Disallow if either piece has already been moved. */
        (king, rook).foreach { p =>
          conf.getExistingPiece(p) match {
            case (_, _, Some(_)) => throw new PreviouslyMovedException(move)
            case default => ()
          }
        }

        /* Disallow if there are any pieces between the rook and king */
        val interveningPositions = Position.getInterveningPositions(king, rook)
        interveningPositions.foreach { p =>
          if (conf.getPiece(p).isDefined) throw new InterveningPieceException(move, p)
        }

        /* Disallow if King is in check or would cross any square that is is attacked or would end in check. */
        // TODO: Consider converting to map operation with predicate to test for attacked status
        val exposedPositions = king :: kingEnd :: Position.getInterveningPositions(king, kingEnd) toSet
        val opponentPositions = conf locatePieces colour.opposite
        opponentPositions.foreach { p =>
          val attackedPositions = getAttackedPositions(p)
          val i = exposedPositions intersect attackedPositions
          if (i.nonEmpty) {
            throw new AttackedPositionException(move, i.head)
          }
        }
      case m @ Promote(start, piece) =>
        checkReachable(start, m.end)
        /* PromoteCapturing must be used when capturing */
        checkNotCapturing(m.end)
        /* Perform this check after all move validating checks */
        checkKingNotLeftInCheckAfterMove(m)
      case m @ PromoteCapturing(start, end, piece) =>
        checkReachable(start, end)
        /* Promote must be used when not capturing */
        checkCapturing(end)
        /* Perform this check after all move validating checks */
        checkKingNotLeftInCheckAfterMove(m)
      case m @ EnPassant(start, end) =>
        checkReachable(start, end)
        checkKingNotLeftInCheckAfterMove(m)
      case _ => throw new UnhandledCaseException(move.toString)
    }
  }

  def kingInCheck(colour: Colour): Boolean = {
    val kings = conf.locatePieces(colour, King)
    assert(kings.size == 1, "One king should be present for: " + colour + " but these kings were found: " + kings)
    val king = kings.head
    val opponentPositions = conf.locatePieces(colour.opposite)
    opponentPositions.exists(p =>
      getBasicPositions(p) contains king)
  }

  def kingInCheckMate(colour: Colour): Boolean = {
    kingInCheck(colour) && !checkedKingCanEscape(colour)
  }

  /*
   * @return true when there is at least one move that will get the king out of
   * check
   */
  // TODO: Ensure checkedKingCanEscape logic is not duplicated elsewhere
  private def checkedKingCanEscape(colour: Colour): Boolean = {
    legalMoves(colour).nonEmpty
  }

  private def checkKingNotLeftInCheckAfterMove(move: SimpleMove): Unit = {
    /*
	   * 1. Clone the current conf
	   * 2. Apply the move without recursively calling this method
	   * 3. See if the King is in check
	   */
    val (colour, _, _) = conf.getExistingPiece(move.start)
    val future = conf.applied(move)

    if (new StandardMoveExplorer(future).kingInCheck(colour)) {
      throw new CheckedOwnKing(move)
    }
  }

  /** @return All legal moves. */
  def legalMoves(colour: Colour): List[Move] = {
    val startPositions = conf.locatePieces(colour)
    var moves = List[Move]()
    for (s <- startPositions) {
      val endPositions = this.getBasicPositions(s)
      val (_, piece, _) = conf.getExistingPiece(s)
      for (moveList <- generateMoves(piece, s, endPositions))
        moves = moveList ::: moves
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
    moves filter { moveAcceptable }
  }

  private def isHomeRow(row: Int): Boolean = List(Constants.WHITE_HOME_ROW, Constants.BLACK_HOME_ROW) contains row
  private def isDiagonal(a: Position, b: Position): Boolean = a.col != b.col

  private val promotionPieces = List(Knight, Queen)

  private def generateMoves(piece: Piece, start: Position, endPositions: Set[Position]) = {
    for (end <- endPositions) yield {
      val endOccupied = conf.getPiece(end).isDefined
      piece match {
        case Pawn if isHomeRow(end.getRow) =>
          if (endOccupied) promotionPieces.map { PromoteCapturing(start, end, _) } else promotionPieces.map { Promote(start, _) }
        case Pawn if !endOccupied && isDiagonal(start, end) => List(EnPassant(start, end))
        case default => if (endOccupied) List(MovePieceCapturing(start, end)) else List(MovePiece(start, end))
      }
    }
  }

  private def moveAcceptable(move: Move): Boolean = {
    try {
      // TODO: Convert rejectIllegalMove to a query method
      this.rejectIllegalMove(move)
      true
    } catch {
      case e: IllegalMoveException => false
    }
  }

  private def log(message: String): Unit = println(message)

}