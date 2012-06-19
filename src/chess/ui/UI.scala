package chess.ui

import chess.model.{ BoardModel, BoardChangedSubscriber, Configuration, GridConfiguration, Colour, Move, Piece, Position, Promote, Resign, WinModes }
import chess.model.Colours.{ Black, White }
import chess.model.{ Rook, Knight, Bishop, Queen, King, Pawn }
import chess.model.{ Castle, MovePiece }
import chess.model.{ BoardChanged, Castled, PieceMoved, PieceMovedCapturing, PiecePlaced, Promoted, Resigned, Won }
import chess.util.TODO	
import chess.model.Stalemated

class UI extends BoardChangedSubscriber {

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
      case Stalemated() => {
        display("Stalemate!")
      }
      case default => {
        TODO.throwRuntimeEx(event.toString)
      }
    }
    render
    moveCount += 1
    display("UI: Move completed: " + moveCount + ": " + event.toString)
    display("")
  }

  // TODO: UI: Visualise board and pieces
  def showBoard {
    for (row <- conf.getRows) {
      for (square <- row) {
        print("|_")
      }
      println
    }
  }

  val symbols = Map[Piece, String](Rook() -> "R", Knight() -> "N", Bishop() -> "B", King() -> "K", Queen() -> "Q", Pawn() -> "P")

  /* Black is lowercase */
  private def render: Unit = {
    def printColumnLabels = println("  abcdefgh")
    printColumnLabels
    val rows = conf.getRows.reverse
    var rowNum = rows.size
    for (row <- rows) {
      print(rowNum + " ")
      rowNum = rowNum - 1
      for (square <- row) {
        val symbol =
          square match {
            /* Middle dot: U+00B7 */
            case null => "·"
            case (c: Colour, p: Piece) => {
              val col = colourise(c)_
              symbols.get(p) match { case Some(s) => col(s) case None => assert(false) }
            }
            case _ => "?"
          }
        print(symbol)
      }
      println
    }
    printColumnLabels
    println
  }

  private def colourise(c: Colour)(symbol: String): String = {
    c match { case White => symbol case Black => symbol.toLowerCase }
  }

  private def display(s: String): Unit = { println(s) }

  val conf: Configuration = new GridConfiguration
}