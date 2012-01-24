package chess.model

/**
 * The moves of standard chess
 */
class StandardMoveExplorer(conf: Configuration) extends MoveExplorer {

  /**
   * @return The a list of possible positions excluding moves that would result in 1. the move escaping from the board edges,
   * or 2. A non-Knight jumping over a piece, or 3. A piece taking another piece of the same colour
   */
  def getBasicPositions(position: Position): List[Position] = {
    val (colour, piece, _) = conf.getExistingPiece(position)

    val vectors = piece.movements(colour)
    // TODO: Move repeatable property into vectors by pre-repeating the vectors
    val repeatable = piece match {
      case Queen() => true
      case Bishop() => true
      case Rook() => true
      case default => false
    }
    var basicPositions = List[Position]()

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
          if (!ownPiece) basicPositions = c :: basicPositions
          if (ownPiece || opponentPiece) advanceTerminated = true
        }
      }
    }
    basicPositions
  }

  private def anyMoveAllowed(startPosition: Position, d: (Int, Int)) = true

  // TODO: Add prerequisites for moves:
  // TODO: Allow en-passant possibly by modelling a takable virtual pawn with Configuration.getTakable
  private def pawnMoveAllowed(startPosition: Position, d: (Int, Int)) = {
    val (dCol, dRow) = d
    if (pawnDiagonal(dCol, dRow)) {
      /* Can only take if piece present. The client code will check for the colour */
      val p = startPosition.offset(dCol, dRow)
      conf.getPiece(p) match {
        case Some(_) => true
        case None => false
      }
    } else if (pawnForward(dCol, dRow)) {
      /* Prevent forward capturing. */
      val p = startPosition.offset(dCol, dRow)
      conf.getPiece(p) match {
        case Some(_) => false
        case None => true
      }
    } else if (pawnForwardTwo(dCol, dRow)) {
      conf.getExistingPiece(startPosition) match {
        /* Disallow double advancement if the pawn has already been moved. */
        case (_, _, Some(_)) => false
        case default => {
          /* Deny attempt to jump over a piece */
          val r = if (dRow == 2) 1 else -1
          val p = startPosition.offset(0, r)
          conf.getPiece(p) match {
            case Some(_) => false
            case None => true
          }
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

}