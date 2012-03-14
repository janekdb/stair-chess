package chess.ui

import chess.model.{ BoardModel, BoardChangedSubscriber, Configuration, GridConfiguration, Colour, Move, Piece, Position, Promote, Resign, WinModes }
import chess.model.Colours.{ Black, White }

import chess.model.{ Rook, Knight, Bishop, Queen, King, Pawn }
import chess.model.{ Castle, MovePiece }

import chess.model.{ BoardChanged, Castled, PieceMoved, PieceMovedTaking, PiecePlaced, Promoted, Resigned, Won }

import chess.util.TODO	

class UI extends BoardChangedSubscriber {

  def onBoardChanged(event: BoardChanged): Unit = {
    event match {
      case Castled(king, rook) => {
        {
          conf.move(king.start, king.end)
        }
        {
          conf.move(rook.start, rook.end)
        }
      }
      case PieceMoved(start, end) => {
        /* Assumes a piece is present at the start position. */
        conf.move(start, end)
      }
      case PieceMovedTaking(start, end, taken) => {
        /* Assumes pieces are present at the start and taken positions. */
        conf.remove(taken)
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
          // TODO: Improve resignation visualization
          case List(position) => { conf.remove(position) }
          case default => throw new RuntimeException("More than one King was found for " + colour + ": " + positions)
        }
      }
      case Won(winner, winMode) => {
        // TODO: UI: Visualize won game
        display(winMode + "! " + winner + " wins")
      }
      case default => {
        TODO.throwRuntimeEx
      }
    }
    render
    display("UI: Move completed: " + event.toString)
    display("")
  }

  // TODO: UI: Visualise board and pieces
  def showBoard: Unit = {
    for (row <- conf.getRows) {
      for (square <- row) {
        print("|_")
      }
      println
    }
  }

  // TODO: Use Piece -> Symbol map
  //  val blackSymbols = Map[Piece, String](Rook -> "r", Knight -> "k", Bishop -> "b", King -> "k", Queen -> "q", Pawn -> "p");
  //  val whiteSymbols = Map(Rook -> "R", Knight -> "K", Bishop -> "B", King -> "K", Queen -> "Q", Pawn -> "P");
  //  val symbols = Map(Black -> blackSymbols, White -> whiteSymbols)

  /* Black is lowercase */
  private def render: Unit = {
    println("  abcdefgh")
    var rowNum = 1
    for (row <- conf.getRows) {
      print(rowNum + " ")
      rowNum = rowNum + 1
      for (square <- row) {
        val symbol =
          square match {
            /* Middle dot: U+00B7 */
            case null => "·"
            case (c: Colour, p: Piece) => {
              val col = colourise(c)_
              p match {
                case (p: Rook) => col("R")
                case (p: Knight) => col("N")
                case (p: Bishop) => col("B")
                case (p: King) => col("K") // Appeared as a question mark: "\u2654"
                case (p: Queen) => col("Q")
                case (p: Pawn) => col("P")
              }
            }
            case _ => "?"
          }
        print(symbol)
      }
      println
    }
    println("  abcdefgh")
    println
  }

  private def colourise(c: Colour)(symbol: String): String = {
    c match { case White => symbol case Black => symbol.toLowerCase }
  }

  private def display(s: String): Unit = { println(s) }

  val conf: Configuration = new GridConfiguration
}