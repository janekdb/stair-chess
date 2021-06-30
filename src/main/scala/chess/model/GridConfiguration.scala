package chess.model

class GridConfiguration extends Configuration {

  var pieces: Map[Position, Placed] = Map[Position, Placed]()

  def add(position: Position, colour: Colour, piece: Piece): Unit = {
    pieces += (position -> Placed(colour, piece, None))
    assert(pieces contains new Position(position.getCol, position.getRow))
  }

  def remove(position: Position, hint: String): Unit = {
    if (!pieces.contains(position)) {
      throw new IllegalStateException(
        s"No piece at $position. Hint: $hint\nPieces: \n"+ listPieces.mkString("\n"))
    }
    pieces -= position
  }

  private def listPieces: List[(Position, Placed)] =
    pieces.toList.sortBy{case (p, _) => p.toString()}

  /** Move the piece, recording its previous position. Throw exception if there is no piece at the given position. Throw
    * exception if the end position is already occupied.
    */
  def move(start: Position, end: Position): Unit = {
    val Placed(colour, piece, _) = getExistingPiece(start)
    if (pieces contains end) {
      throw new IllegalStateException("End position was occupied: " + end)
    }
    pieces -= start
    pieces += (end -> Placed(colour, piece, Some(start)))
    lastMove = Option((piece, start, end))
  }

  private var lastMove: Option[(Piece, Position, Position)] = None

  def getLastMove: Option[(Piece, Position, Position)] = lastMove

  /** Replace the piece with a different piece of the same colour carrying over the previous position */
  def replace(position: Position, replacementPiece: Piece): Unit = {
    val Placed(colour, piece, ppo) = getExistingPiece(position)
    if(replacementPiece==piece){
      throw new IllegalArgumentException(s"Attempt to replace piece with same piece: $position, $piece, $pieces")
    }
    pieces += (position -> Placed(colour, replacementPiece, ppo))
  }

  def getRows: List[List[(Colour, Piece)]] = {
    val rows: Seq[List[(Colour, Piece)]] =
      for (r <- 1 to Constants.BOARD_SIZE) yield {
        val row: Seq[(Colour, Piece)] =
          for (c <- 1 to Constants.BOARD_SIZE) yield {
            pieces.get(new Position(c, r)) match {
              case Some(Placed(colour, piece, _)) => (colour, piece)
              case None                     => null
            }
          }
        row.toList
      }
    rows.toList
  }

  /** Throw exception if there is no piece at the given position */
  def getExistingPiece(position: Position): Placed = {
    pieces.get(position) match {
      case Some(p) => p
      case None =>
        println(pieces)
        throw new IllegalStateException("No piece at " + position)
    }
  }

  def getPiece(position: Position): Option[Placed] = pieces.get(position)

  def locatePieces(colour: Colour, piece: Piece): List[Position] = {
    for ((position, Placed(c, pi, _)) <- pieces.toList; if c == colour && pi == piece) yield position
  }

  /** Return positions of all pieces of the given colour. */
  def locatePieces(colour: Colour): List[Position] = {
    for ((position, Placed(c, _, _)) <- pieces.toList; if c == colour) yield position
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
