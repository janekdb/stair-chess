package chess.model

trait CastlingType {
  def getPositions(row: Int): ((Position, Position), (Position, Position)) = {
    val ((ks, ke), (rs, re)) = getColumns
    def p(s: Int, e: Int) = (new Position(s, row), new Position(e, row))
    (p(ks, ke), p(rs, re))
  }

  /* ((king start, king end), (rook start, rook end)) */
  def getColumns: ((Int, Int), (Int, Int))
}

object Short extends CastlingType {
  def getColumns = ((5, 7), (8, 6))
  override def toString = "Short"
}

object Long extends CastlingType {
  def getColumns = ((5, 3), (1, 4))
  override def toString = "Long"
}
