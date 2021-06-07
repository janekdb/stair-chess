package chess.model

sealed abstract class Piece {
  def movements: List[(Int, Int)] = List()

  def movements(colour: Colour): List[(Int, Int)] = movements

  def value: Int = ???
}

// TODO: Convert pieces to case objects
case object Pawn extends Piece {
  override def movements(colour: Colour): List[(Int, Int)] = {
    val s = colour.pawnRowIncrement
    List((0, 1), (0, 2), (-1, 1), (1, 1)) map (t => (t._1, s * t._2))
    List((0, 1), (0, 2), (-1, 1), (1, 1)) map { case (c, r) => (c, s * r) }
  }

  override def value: Int = 1
}

case object Knight extends Piece {
  override def movements  = List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-1, 2), (-2, 1))
  override def value: Int = 2
}

case object Bishop extends Piece {
  override def movements  = List((1, 1), (1, -1), (-1, -1), (-1, 1))
  override def value: Int = 3
}

case object Rook extends Piece {
  override def movements  = List((1, 0), (0, -1), (-1, 0), (0, 1))
  override def value: Int = 4
}

case object Queen extends Piece {
  override def movements  = List((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
  override def value: Int = 5
}

case object King extends Piece {
  override def movements  = List((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
  override def value: Int = 6
}
