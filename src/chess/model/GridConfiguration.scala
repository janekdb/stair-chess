package chess.model

//import Colours.Colour
import chess.util.TODO
import scala.collection.mutable

class GridConfiguration extends Configuration {

  var pieces = mutable.Map[Position, (Colour, Piece, Option[Position])]()

  def add(position: Position, colour: Colour, piece: Piece): Unit = {
    pieces += (position -> (colour, piece, None))
    assert(pieces contains new Position(position.getCol, position.getRow))
  }

  def remove(position: Position): Unit = {
    val option = pieces.get(position)
    pieces.remove(position) match {
      case Some(_) => Unit
      case None => {
        println(pieces)
        throw new IllegalStateException("No piece at " + position)
      }
    }
  }

  /**
   * Move the piece, recording its previous position.
   * Throw exception if there is no piece at the given position.
   * Throw exception if the end position is already occupied.
   */
  def move(start: Position, end: Position): Unit = {
    val (colour, piece, _) = getExistingPiece(start)
    if (pieces contains end) {
      throw new IllegalStateException("End position was occupied: " + end)
    }
    pieces.remove(start)
    pieces += (end -> (colour, piece, Some(start)))
    lastMove = Option((piece, start, end))
  }

  private var lastMove: Option[(Piece, Position, Position)] = None

  // TODO: Drop getLastMove in favour of compiler provided getter
  def getLastMove: Option[(Piece, Position, Position)] = lastMove

  /** Replace the piece with a the same colour carrying over the previous position */
  def replace(position: Position, replacementPiece: Piece): Unit = {
    val (colour, _, ppo) = getExistingPiece(position)
    pieces += (position -> (colour, replacementPiece, ppo))
  }

  def getRows: List[List[(Colour, Piece)]] = {
    var rows = List[List[(Colour, Piece)]]()
    for (r <- 1 to Constants.BOARD_SIZE) {
      var row = List[(Colour, Piece)]()
      for (c <- 1 to Constants.BOARD_SIZE) {
        pieces.get(new Position(c, r)) match {
          case Some((colour, piece, _)) => row = (colour, piece) :: row
          case None => row = null :: row
        }
      }
      rows = row.reverse :: rows
    }
    rows.reverse
  }

  /** Throw exception if there is no piece at the given position */
  def getExistingPiece(position: Position): (Colour, Piece, Option[Position]) = {
    pieces.get(position) match {
      case Some(p) => p
      case None => {
        println(pieces)
        throw new IllegalStateException("No piece at " + position)
      }
    }
  }

  def getPiece(position: Position): Option[(Colour, Piece, Option[Position])] = pieces.get(position)

  /** Return positions of all pieces of the given colour and type. */
  // TODO: Convert this to use a closure with foreach and yield
  def locatePieces(colour: Colour, piece: Piece): List[Position] = {
    var result = List[Position]()
    for ((position, (c, pi, _)) <- pieces; if c == colour && pi == piece) {
      result = position :: result
      //      yield position
    }
    result
  }

  /** Return positions of all pieces of the given colour. */
  // TODO: Convert this to use a closure with foreach and yield
  def locatePieces(colour: Colour): List[Position] = {
    var result = List[Position]()
    for ((position, (c, _, _)) <- pieces; if c == colour) {
      result = position :: result
    }
    result
  }

  /** Return a deep copy of the Configuration */
  def copyOf: Configuration = {
    val c = new GridConfiguration
    for ((position, (colour, piece, last)) <- pieces) {
      c.pieces += (position -> (colour, piece, last))
    }
    /* Immutable object */
    c.lastMove = lastMove
    c
  }

  //  private def dumpGrid = {
  //    for (r <- getRows)
  //      println(r mkString ",")
  //  }
}