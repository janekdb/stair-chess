package chess.model

sealed trait Colour {
  def opposite: Colour
  def homeRow: Int
  def pawnRowIncrement: Int
  def enPassantRow: Int
}

object Colours {

  case object White extends Colour {
    val opposite = Black
    val homeRow = Constants.WHITE_HOME_ROW
    val pawnRowIncrement = 1
    val enPassantRow = 5
  }

  case object Black extends Colour {
    val opposite = White
    val homeRow = Constants.BLACK_HOME_ROW
    val pawnRowIncrement = -1
    val enPassantRow = 4
  }

}