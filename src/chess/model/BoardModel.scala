package chess.model

import chess.util.UnhandledCaseException
import chess.util.TODO
import chess.model.ex.{
  //  AttackedPositionException,
  //  CheckedOwnKing,
  //  InterveningPieceException,
  IllegalMoveException
  //  PreviouslyMovedException,
  //  UnreachablePositionException
}
import Misc.kingInCheck
import WinModes.WinMode

/**
 * This class is concerned with maintaining a model of a chess game. It contains no UI. A UI can be attached as a listener.
 *
 * With White at the top of a grid that hangs down the coordinate of a square is (column 1-8, row 1-8).
 */
class BoardModel {

  var winner: Colour = null

  var winMode: WinModes.WinMode = null

  private def wonGuard = if (!isWon) throw new AssertionError("There is no winner")

  def isWon = winner != null

  def getWinner: Colour = {
    winner ensuring (isWon)
  }

  // TODO: Move isCheckMate into WinState object
  def isCheckMate: Boolean = {
    winMode == WinModes.CheckMate ensuring (isWon)
  }

  //  TODO: Move isResigned into WinState object
  def isResigned: Boolean = {
    winMode == WinModes.Resignation ensuring (isWon)
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

  // TODO: Encapsulate the win state into an object
  private def setWinState(winMode: WinMode, winner: Colour) {
    this.winMode = winMode
    this.winner = winner
  }
    
  def move(move: Move): Unit = {
    debug("Moving: " + move)

    if (winner != null) {
      throw new IllegalStateException("The game has already been won");
    }
    moveExplorer.rejectIllegalMove(move)
    val events = move match {
      case Resign(colour) => {
        setWinState(WinModes.Resignation, colour.opposite)
        List(Resigned(colour))
      }
      case default => {
        /* Cache off the colour before the move is applied. */
        val colour = extractColour(move)
        val e = conf.applyMove(move)

        // TODO: Move this side-effect out of the case statement
        if (checkForCheckMate(colour.opposite)) {
        	setWinState(WinModes.CheckMate, colour)
        }
        e
      }
    }

    for (s <- subscribers; e <- events) { s.onBoardChanged(e) }
    // TODO: Consider including Won(_, _) in the list of events from applyMove
    if (winner != null) {
      // TODO: Change this to use the WinState object
      subscribers.foreach { _.onBoardChanged(Won(winner, winMode)) }
    }
  }

  private def extractColour(move: Move): Colour = {
    // TODO: Reduce the amount of code used to acquire the active colour.
    move match {
      case Castle(colour, _) => colour
      case MovePiece(start, _) => {
        val (colour, _, _) = conf.getExistingPiece(start)
        colour
      }
      case Promote(start, _, _) => {
        val (colour, _, _) = conf.getExistingPiece(start)
        colour
      }
      case EnPassant(start, _) => {
        val (colour, _, _) = conf.getExistingPiece(start)
        colour
      }
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

  private def checkForCheckMate(colour: Colour): Boolean = {
    return kingInCheck(colour, conf) && !checkedKingCanEscape(colour, conf)
  }

  /*
   * @return true when there is at least one move that will get the king out of
   * check
   */
  private def checkedKingCanEscape(colour: Colour, conf: Configuration): Boolean = {
    val me = new StandardMoveExplorer(conf)
    def moveIsLegal(move: MovePiece) = {
      try {
        // TODO: Convert from exception throwing to case class return to signal illegal move
        me.rejectIllegalMove(move)
        /* The move did not leave the king in check so there is a way out of check */
        true
      } catch {
        case e: IllegalMoveException => { false };
      }
    }
    conf.locatePieces(colour).exists(start =>
      me.getBasicPositions(start).exists(end => moveIsLegal(MovePiece(start, end))))
  }

  // Debug
  private def debug(s: String): Unit = { println(s) }

  // Events

  var subscribers: List[BoardChangedSubscriber] = Nil

  def subscribe(subscriber: BoardChangedSubscriber) = subscribers ::= subscriber

}