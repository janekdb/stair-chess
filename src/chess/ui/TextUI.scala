package chess.ui

import chess.model.{ BoardModel, BoardChangedSubscriber, Configuration, ConfigurationView, GridConfiguration, Colour, Move, Piece, Position, Promote, Resign, GameOutcomeModes }
import chess.model.{ Rook, Knight, Bishop, Queen, King, Pawn }
import chess.model.{ Castle, MovePiece }
import chess.model.{ BoardChanged, Castled, PieceMoved, PieceMovedCapturing, PiecePlaced, Promoted, Resigned }
import chess.model.GameChanged
import chess.model.GameChangedSubscriber
import chess.model.Won
import chess.model.Drawn

class TextUI extends BoardChangedSubscriber with GameChangedSubscriber {

  private var moveCount = 0

  def onGameChanged(event: GameChanged) {

    event match {
      case Won(winner, winMode) => {
        // TODO: UI: Visualize won game
        display(winMode + "! " + winner + " wins")
      }
      case Drawn(drawMode) => {
        display(drawMode.toString)
      }
    }
  }

  def onBoardChanged(events: List[BoardChanged]) {
    events foreach onBoardChanged _
    moveCount += 1
    display("TextUI: Move completed: " + moveCount + ": " + events.toString)
    display("")
  }

  private def onBoardChanged(event: BoardChanged) {

    event match {
      case Castled(king, rook) => {
          conf.move(king.start, king.end)
          conf.move(rook.start, rook.end)
      }
      case PieceMoved(start, end) => {
        /* Assumes a piece is present at the start position. */
        conf.move(start, end)
      }
      case PieceMovedCapturing(start, end, captured) => {
        /* Assumes pieces are present at the start and captured positions. */
        conf.remove(captured)
        conf.move(start, end)
      }
      case Promoted(position, replacementPiece) => {
        conf.replace(position, replacementPiece)
      }
      case Resigned(colour) => {
        val positions = conf.locatePieces(colour, King)
        positions match {
          case List() => throw new RuntimeException("The King could not be found for " + colour)
          // TODO: UI: Improve resignation visualization
          case List(position) => { conf.remove(position) }
          case default => throw new RuntimeException("More than one King was found for " + colour + ": " + positions)
        }
      }
      case default => throw new AssertionError("event was not handled: " + event)
    }
    render
  }

  def onPiecePlaced(event: PiecePlaced) {
    conf.add(event.position, event.colour, event.piece)
    render
  }

  private def render {
    val lines = ConfigurationView.getTextRepresentation(conf)
    for (line <- lines) println(line)
    println
  }

  private def display(s: String): Unit = { println(s) }

  val conf: Configuration = new GridConfiguration
}