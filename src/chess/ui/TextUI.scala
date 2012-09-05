package chess.ui

import chess.model.{ BoardModel, BoardChangedSubscriber, Configuration, ConfigurationView, GridConfiguration, Colour, Move, Piece, Position, Promote, Resign, GameOutcomeModes }
import chess.model.{ Rook, Knight, Bishop, Queen, King, Pawn }
import chess.model.{ Castle, MovePiece }
import chess.model.{ BoardChanged, Castled, PieceMoved, PieceMovedCapturing, PiecePlaced, Promoted, Resigned, Won }
import chess.util.TODO
import chess.model.Drawn

class TextUI extends BoardChangedSubscriber {

  private var moveCount = 0

  def onBoardChanged(event: BoardChanged): Unit = {

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
      case PiecePlaced(colour, piece, position) => {
        conf.add(position, colour, piece)
      }
      case Promoted(position, replacementPiece) => {
        conf.replace(position, replacementPiece)
      }
      case Resigned(colour) => {
        val positions = conf.locatePieces(colour, King())
        positions match {
          case List() => throw new RuntimeException("The King could not be found for " + colour)
          // TODO: UI: Improve resignation visualization
          case List(position) => { conf.remove(position) }
          case default => throw new RuntimeException("More than one King was found for " + colour + ": " + positions)
        }
      }
      case Won(winner, winMode) => {
        // TODO: UI: Visualize won game
        display(winMode + "! " + winner + " wins")
      }
      case Drawn(drawMode) => {
        display(drawMode.toString)
      }
      case default => {
        TODO.throwRuntimeEx(event.toString)
      }
    }
    render
    // TODO: ->Stop special casing Promoted and PiecePlaced by adding a move count subscriber or separating PiecePlaced, Won, Drawn into a new type to allow a new subscriber
    event match {
      case _ : Promoted => Unit
      case _ : PiecePlaced => Unit
      case _ : Won => Unit
      case _ : Drawn => Unit
      case default => moveCount += 1
    }
    display("TextUI: Move completed: " + moveCount + ": " + event.toString)
    display("")
    // TODO: --->Remove this
    if (event.isInstanceOf[Won]) {
      TODO.throwRuntimeEx("Remove once it has been determined moveCount is incorrect")
    }

  }

  private def render {
    val lines = ConfigurationView.getTextRepresentation(conf)
    for (line <- lines) println(line)
    println
  }

  private def display(s: String): Unit = { println(s) }

  val conf: Configuration = new GridConfiguration
}