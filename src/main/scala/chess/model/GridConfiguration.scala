package chess.model

//import Colours.Colour
import chess.util.TODO

class GridConfiguration extends Configuration {

  var pieces: Map[Position, (Colour, Piece, Option[Position])] = Map[Position, (Colour, Piece, Option[Position])]()

  def add(position: Position, colour: Colour, piece: Piece): Unit = {
    pieces += (position -> (colour, piece, None))
    assert(pieces contains new Position(position.getCol, position.getRow))
  }

  def remove(position: Position): Unit = {
    if (!pieces.contains(position)) {
      throw new IllegalStateException("No piece at " + position + ". Pieces: " + pieces)
    }
    pieces -= position
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
    pieces -= start
    pieces += (end -> (colour, piece, Some(start)))
    lastMove = Option((piece, start, end))
  }

  private var lastMove: Option[(Piece, Position, Position)] = None

  def getLastMove: Option[(Piece, Position, Position)] = lastMove

  /** Replace the piece with a the same colour carrying over the previous position */
  def replace(position: Position, replacementPiece: Piece): Unit = {
    val (colour, _, ppo) = getExistingPiece(position)
    pieces += (position -> (colour, replacementPiece, ppo))
  }

  def getRows: List[List[(Colour, Piece)]] = {
    val rows: Seq[List[(Colour, Piece)]] =
      for (r <- 1 to Constants.BOARD_SIZE) yield {
        val row: Seq[(Colour, Piece)] =
          for (c <- 1 to Constants.BOARD_SIZE) yield {
            pieces.get(new Position(c, r)) match {
              case Some((colour, piece, _)) => (colour, piece)
              case None => null
            }
          }
        row.toList
      }
    rows.toList
  }

  /** Throw exception if there is no piece at the given position */
  def getExistingPiece(position: Position): (Colour, Piece, Option[Position]) = {
    pieces.get(position) match {
      case Some(p) => p
      case None =>
        println(pieces)
        throw new IllegalStateException("No piece at " + position)
    }
  }

  def getPiece(position: Position): Option[(Colour, Piece, Option[Position])] = pieces.get(position)

  def locatePieces(colour: Colour, piece: Piece): List[Position] = {
    for ((position, (c, pi, _)) <- pieces.toList; if c == colour && pi == piece) yield position
  }

  /** Return positions of all pieces of the given colour. */
  def locatePieces(colour: Colour): List[Position] = {
    for ((position, (c, _, _)) <- pieces.toList; if c == colour) yield position
  }

  /** Return a deep copy of the Configuration */
  def copyOf: Configuration = {
    val c = new GridConfiguration
    /* Immutable object */
    c.pieces = pieces
    /* Immutable object */
    c.lastMove = lastMove
    c
  }

  def applied(move: Move): ConfigurationView = {
    val conf = copyOf
    conf.applyMove(move)
    new DelegatingConfigurationView(conf)
  }

  //  private def dumpGrid = {
  //    for (r <- getRows)
  //      println(r mkString ",")
  //  }
}