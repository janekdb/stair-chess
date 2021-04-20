package chess.model

sealed trait Colour {
  def opposite: Colour
  def homeRow: Int
  def pawnRowIncrement: Int
  def enPassantRow: Int
}

object Colours {

  case object White extends Colour {
    val opposite: Colour = Black
    val homeRow: Int = Constants.WHITE_HOME_ROW
    val pawnRowIncrement = 1
    val enPassantRow = 5
  }

  case object Black extends Colour {
    val opposite: Colour = White
    val homeRow: Int = Constants.BLACK_HOME_ROW
    val pawnRowIncrement: Int = -1
    val enPassantRow = 4
  }

}