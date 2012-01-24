package chess.model

import chess.util.UnhandledCaseException
import chess.util.TODO
import chess.model.ex.{
  AttackedPositionException,
  CheckedOwnKing,
  InterveningPieceException,
  PreviouslyMovedException,
  UnreachablePositionException
}

/**
 * This class is concerned with maintaining a model of a chess game. It contains no UI. A UI can be attached as a listener.
 *
 * With White at the top of a grid that hangs down the coordinate of a square is (column 1-8, row 1-8).
 */
class BoardModel {

  implicit def tuple2list(t: Tuple2[Position, Position]) = List(t._2, t._2)

  var winner: Colour = null

  var winMode: WinModes.WinMode = null

  private def wonGuard = if (!isWon) throw new AssertionError("There is no winner")

  def isWon = winner != null

  def getWinner: Colour = {
    winner ensuring(isWon)
  }

  def isCheckMate: Boolean = {
    winMode == WinModes.CheckMate ensuring(isWon)
  }

  def isResigned: Boolean = {
    winMode == WinModes.Resignation ensuring(isWon)
  }

  /* TODO: Hide this var from the test */
  var placed: Boolean = false

  def placePieces: Unit = {
    if (placed) {
      throw new IllegalStateException("The pieces have been placed");
    }

    val pawns = List.fill(Constants.BOARD_SIZE)(Pawn())
    val others = List(Rook(), Knight(), Bishop(), King(), Queen(), Bishop(), Knight(), Rook())

    place(Colours.White, others, 1)
    place(Colours.White, pawns, 2)

    place(Colours.Black, pawns, 7)
    place(Colours.Black, others, 8)

    placed = true
  }

  private def place(colour: Colour, pieces: List[Piece], row: Int): Unit = {
    place(colour, pieces, new Position(1, row))
  }

  private val conf: Configuration = new GridConfiguration
  private val moveExplorer: MoveExplorer = new StandardMoveExplorer(conf)
  
  /* TODO: Revert to private or extract move checking to new object */
   def place(colour: Colour, pieces: List[Piece], position: Position): Unit = {

    val p :: ps = pieces
    conf.add(position, colour, p)
    subscribers.foreach { _.onBoardChanged(PiecePlaced(colour, p, position)) }
    if (!ps.isEmpty) {
      place(colour, ps, position.incrementCol)
    }
  }

  def move(move: Move): Unit = {
    debug("Moving: " + move)
    
    if (winner != null) {
      throw new IllegalStateException("The game has already been won");
    }
    rejectIllegalMove(move)
    val events = move match {
      case Resign(colour) => {
        // TODO: Set the win mode and colour via a private method
        winMode = WinModes.Resignation
        winner = colour.opposite
        List(Resigned(colour))
      }
      case default => {
        // TODO: Reduce the amount of code used to acquire the active colour.
        val colour = move match {
          case Castle(colour, _) => colour
          case MovePiece(start, _) => {
            val (colour, _, _) = conf.getExistingPiece(start)
            colour
          }
          case Promote(start, _, _) => {
            val (colour, _, _) = conf.getExistingPiece(start)
            colour
          }
          case default => throw new UnhandledCaseException(move.toString)
        }

        val e = conf.applyMove(move)
        
        // TODO: Move this side-effect out of the case statement
        if (checkForCheckMate(colour.opposite)) {
          winMode = WinModes.CheckMate
          winner = colour
        }
        e
      }
    }

    for (s <- subscribers; e <- events) { s.onBoardChanged(e) }
    // TODO: Consider including Won(_, _) in the list of events from applyMove
    if (winner != null) {
      subscribers.foreach { _.onBoardChanged(Won(winner, winMode)) }
    }
  }
  
  private def rejectIllegalMove(move: Move) {
    def checkReachable(start: Position, end: Position) = {
      val legalPositions = moveExplorer.getBasicPositions(start)
      if (!legalPositions.contains(end)) {
        throw new UnreachablePositionException(MovePiece(start, end), legalPositions)
      }
    }
    def checkKingNotLeftInCheckAfterMove(start: Position, end: Position) = {

      /*
       * 1. Clone the current conf
       * 2. Apply the move without recursively calling this method
       * 3. See if the King is in check
       */

      val (colour, _, _) = conf.getExistingPiece(start)
      val future = conf.copyOf
      future.applyMove(MovePiece(start, end))
      val futureMoveExplorer = new StandardMoveExplorer(future)
      val List(king) = future.locatePieces(colour, King())
      // TODO: Consolidate this code with the same code as the Castling case by passing a block to
      //   code that finds and iterates over the opponenets pieces.
      val opponentPositions = future.locatePieces(colour.opposite)
      opponentPositions.foreach { p =>
        val attackedPositions = futureMoveExplorer.getBasicPositions(p)
        if (attackedPositions.contains(king)) {
          throw new CheckedOwnKing(move)
        }
      }

    }
    move match {
      case MovePiece(start, end) => {
        checkReachable(start, end)
        checkKingNotLeftInCheckAfterMove(start, end)
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

        val ((king, _), (rook, _)) = castlingType.getPositions(row)

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
        val exposedPositions = king :: rook :: interveningPositions
        val opponentPositions = conf.locatePieces(colour.opposite)
        opponentPositions.foreach { p =>
          val attackedPositions = moveExplorer.getBasicPositions(p)
          val i = exposedPositions.intersect(attackedPositions)
          if (i.nonEmpty) {
            throw new AttackedPositionException(move, i.head)
          }
        }

      }
//      case Resign(_) => Unit
      case Promote(start, end, piece) => {
        checkReachable(start, end)
        checkKingNotLeftInCheckAfterMove(start, end)
      }
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

  // TODO: Check for check mate and assign winner if checkmate
  private def checkForCheckMate(colour: Colour): Boolean = false

  // Debug
  private def debug(s: String): Unit = { println(s) }

  // Events

  var subscribers: List[BoardChangedSubscriber] = Nil

  def subscribe(subscriber: BoardChangedSubscriber) = subscribers ::= subscriber

}