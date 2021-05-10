package chess.model

// TODO: Convert to case class
class Position(val col: Int, val row: Int) {

  /* e8 */
  def this(position: String) = this(
    Constants.COLUMN_LABELS.indexOf(position.substring(0, 1)) + 1, position.substring(1, 2).toInt)

  if (row < 1) throw new IllegalArgumentException("row was < 1")
  if (row > Constants.BOARD_SIZE) throw new IllegalArgumentException("row was > " + Constants.BOARD_SIZE + ": " + row)
  if (col < 1) throw new IllegalArgumentException("col was < 1")
  if (col > Constants.BOARD_SIZE) throw new IllegalArgumentException("col was > " + Constants.BOARD_SIZE + ": " + col)

  def incrementCol: Position = new Position(col + 1, row)
  def incrementRow: Position = new Position(col, row + 1)
  def decrementRow: Position = new Position(col, row - 1)

  def offset(dCol: Int, dRow: Int): Position = new Position(col + dCol, row + dRow)

  def getRow: Int = row
  def getCol: Int = col

  private def inBounds(i: Int): Boolean = i >= 1 & i <= Constants.BOARD_SIZE

  /** return true if the position would remain inside the board */
  def canOffset(dCol: Int, dRow: Int): Boolean = inBounds(col + dCol) & inBounds(row + dRow)

  override def toString: String = { Constants.COLUMN_LABELS.substring(col - 1, col) + row }

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Position]) {
      false
    } else {
      val other = that.asInstanceOf[Position]
      this.row == other.row && this.col == other.col
    }
  }

  override def hashCode: Int = row + col
}

object Position {
  
  /**
   * @return all intervening positions.
   * @throws IllegalArgumentException if the positions do not share the same row
   */
  def getInterveningPositions(a: Position, b: Position): List[Position] = {
    if(a.getRow != b.getRow) throw new IllegalArgumentException("The positions must be on the same row")
    if(a.getCol == b.getCol) return List()
    val (min, max) = if(a.getCol < b.getCol)(a.getCol, b.getCol) else (b.getCol, a.getCol)
    /* (Inclusive, Exclusive) */
    for(c <- List.range(min+1, max))
      yield new Position(c, a.getRow)
  }
  
}