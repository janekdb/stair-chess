package chess.model

sealed trait Colour {
  def opposite: Colour
  def homeRow: Int
  def pawnRowIncrement: Int
  def enPassantRow: Int
}

// TODO: Drop Enumeration in exchange for case objects
//sealed abstract class Move
//case object Left  extends Move
//case object Right extends Move

object Colours extends Enumeration {

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