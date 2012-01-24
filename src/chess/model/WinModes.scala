package chess.model

object WinModes extends Enumeration {
  type WinMode = Value
  val Resignation, CheckMate = Value
}