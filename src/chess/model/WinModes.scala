package chess.model

// TODO: Rename WinModes to GameOutcomeModes
object WinModes extends Enumeration {
  type WinMode = Value
  val Resignation, CheckMate, Stalemate = Value
}