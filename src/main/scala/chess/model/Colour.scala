package chess.model

enum Colour:
  case White, Black

  def opposite: Colour = this match
    case White => Black
    case Black => White

  def homeRow: Int = this match
    case White => Constants.WHITE_HOME_ROW
    case Black => Constants.BLACK_HOME_ROW

  def pawnRowIncrement: Int = this match
    case White => 1
    case Black => -1

  def enPassantRow: Int = this match
    case White => 5
    case Black => 4
