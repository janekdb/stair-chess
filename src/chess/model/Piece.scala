package chess.model

sealed abstract class Piece {
  def movements: List[(Int, Int)] = List()
  def movements(colour: Colour): List[(Int, Int)] = movements
}
case class Pawn extends Piece {
  override def movements(colour: Colour): List[(Int, Int)] = {
    val s = colour.pawnRowIncrement
    List((0, 1), (0, 2), (-1, 1), (1, 1)) map (t => (t._1, s * t._2))
  }
}
case class Rook extends Piece { override def movements = List((1, 0), (0, -1), (-1, 0), (0, 1)) }
case class Knight extends Piece { override def movements = List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-1, 2), (-2, 1)) }
case class Bishop extends Piece { override def movements = List((1, 1), (1, -1), (-1, -1), (-1, 1)) }
case class King extends Piece { override def movements = List((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)) }
case class Queen extends Piece { override def movements = List((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)) } 
