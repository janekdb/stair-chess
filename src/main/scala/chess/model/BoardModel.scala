package chess.model

import chess.model.ex.EarlyStalemateException
import chess.model.ex.IllegalMoveException
import chess.model.ex.InvalidStalemateException
import chess.model.ex.UnconsideredMovesStalemateException
import chess.util.UnhandledCaseException
import GameOutcomeModes.GameOutcomeMode
import chess.util.TODO

// TODO: End game when the last n positions have been repeated checking for the value of n
//   when only two kings
//   when stalemate
//		  abcdefgh
//		8 ········
//		7 ········
//		6 ········
//		5 ········
//		4 ······n·
//		3 ········
//		2 ········
//		1 ·····k·K
//		  abcdefgh

/** This class is concerned with maintaining a model of a chess game. It contains no UI. A UI can be attached as a
  * listener.
  *
  * With White at the top of a grid that hangs down the coordinate of a square is (column 1-8, row 1-8).
  */
class BoardModel(
    var boardChangedSubscribers: List[BoardChangedSubscriber],
    var gameChangedSubscribers: List[GameChangedSubscriber]
) {

  private val conf: Configuration        = new GridConfiguration
  private val moveExplorer: MoveExplorer = new StandardMoveExplorer(conf)

  /** @return a copy of the current Configuration */
  def getConfiguration: Configuration = conf.copyOf

  def getMoveExplorer: MoveExplorer = moveExplorer

  def this(
      placements: List[(Colour, Piece, Position)],
      boardChangedSubscribers: List[BoardChangedSubscriber],
      confChangedSubscribers: List[ConfigurationChangedSubscriber],
      gameChangedSubscribers: List[GameChangedSubscriber]
  ) = {
    this(boardChangedSubscribers, gameChangedSubscribers)
    val confView = new DelegatingConfigurationView(conf)
    confChangedSubscribers foreach {
      _.onConfigurationChanged(confView)
    }
    for ((colour, piece, position) <- placements) place(colour, piece, position)
  }

  case class GameOutcome(gameOutcomeMode: GameOutcomeMode, winner: Option[Colour]) {
    def isCheckMate: Boolean = gameOutcomeMode == GameOutcomeModes.CheckMate

    def isResigned: Boolean = gameOutcomeMode == GameOutcomeModes.Resignation

    def isStalemate: Boolean = gameOutcomeMode == GameOutcomeModes.Stalemate
  }

  var gameOutcome: Option[GameOutcome] = None

  private def wonGuard(): Unit = if (!isWon) throw new AssertionError("There is no winner")

  def isWon: Boolean = gameOutcome.isDefined && (gameOutcome.get.isCheckMate || gameOutcome.get.isResigned)

  def isCompleted: Boolean = gameOutcome.isDefined

  def isDrawn: Boolean = gameOutcome.isDefined && gameOutcome.get.isStalemate

  def getGameOutcome: Option[GameOutcome] = gameOutcome

  def getWinner: Option[Colour] = {
    gameOutcome.get.winner
  }

  private def place(colour: Colour, piece: Piece, position: Position): Unit = {
    assert(colour != null)
    assert(piece != null)
    assert(position != null)
    assert(conf != null)
    conf.add(position, colour, piece)
    boardChangedSubscribers.foreach {
      _.onPiecePlaced(PiecePlaced(colour, piece, position))
    }
  }

  private def setGameOutcome(gameOutcome: GameOutcome): Unit = {
    this.gameOutcome = Some(gameOutcome)
  }

  private var lastColour: Option[Colour] = None

  def move(optMove: Option[Move]): Unit = {

    if (gameOutcome.isDefined) {
      throw new IllegalStateException("The game has already been completed")
    }

    if (optMove.isDefined) {
      moveExplorer.rejectIllegalMove(optMove.get)
    } else {
      if (lastColour.isEmpty) throw new EarlyStalemateException
      val moves = moveExplorer.legalMoves(lastColour.get.opposite)
      if (moves.nonEmpty) throw new UnconsideredMovesStalemateException(moves)
    }

    /* Extract the last colour before the configuration is changed. */
    val optColour = optMove match {
      case None => None
      case _    => Some(extractColour(optMove.get))
    }

    val (events: List[BoardChanged], outcomeOpt) = optMove match {
      // TODO: Add a way to resign a game but not by using a Move subclass
      case None =>
        (List(), Some(GameOutcome(GameOutcomeModes.Stalemate, None)))
      case _ =>
        val move   = optMove.get
        val colour = optColour.get
        val e      = conf.applyMove(move)
        val outcomeOption =
          if (checkForCheckMate(colour.opposite)) Some(GameOutcome(GameOutcomeModes.CheckMate, Some(colour))) else None
        (e, outcomeOption)
    }
    if (outcomeOpt.isDefined) {
      setGameOutcome(outcomeOpt.get)
    }
    for (s <- boardChangedSubscribers) {
      s.onBoardChanged(events)
    }

    val wonEvent   = if (isWon) List(Won(gameOutcome.get.winner.get, gameOutcome.get.gameOutcomeMode)) else Nil
    val drawnEvent = if (isDrawn) List(Drawn(GameOutcomeModes.Stalemate)) else Nil
    for (s <- gameChangedSubscribers; e <- Nil ::: wonEvent ::: drawnEvent) {
      s.onGameChanged(e)
    }

    lastColour = optColour
  }

  private def extractColour(move: Move): Colour = {
    implicit def tuple2colour(t: (Colour, Piece, Option[Position])): Colour = t._1

    move match {
      case Castle(colour, _)             => colour
      case MovePiece(start, _)           => conf.getExistingPiece(start)
      case MovePieceCapturing(start, _)  => conf.getExistingPiece(start)
      case Promote(start, _)             => conf.getExistingPiece(start)
      case PromoteCapturing(start, _, _) => conf.getExistingPiece(start)
      case EnPassant(start, _)           => conf.getExistingPiece(start)
      case _                             => throw new UnhandledCaseException(move.toString)
    }
  }

  private def checkForCheckMate(colour: Colour): Boolean = moveExplorer.kingInCheckMate(colour)

  // Debug
  private def debug(s: String): Unit = {
    println(s)
  }

  // Events

  def subscribe(subscriber: BoardChangedSubscriber): Unit = {
    boardChangedSubscribers ::= subscriber
  }

  def subscribe(subscriber: GameChangedSubscriber): Unit = {
    gameChangedSubscribers ::= subscriber
  }

}

object BoardModel {

  import Colours.{Black, White}

  def standardPlacements: List[(Colour, Piece, Position)] = {

    val pawns  = List.fill(Constants.BOARD_SIZE)(Pawn)
    val others = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

    def bp(pieces: List[Piece], colour: Colour, row: Int) = {
      pieces.zipWithIndex.map { case (piece, index) => (colour, piece, new Position(index + 1, row)) }
    }

    bp(others, White, 1) ::: bp(pawns, White, 2) ::: bp(pawns, Black, 7) ::: bp(others, Black, 8)
  }
}
