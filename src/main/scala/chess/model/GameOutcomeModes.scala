package chess.model

object GameOutcomeModes extends Enumeration {
  type GameOutcomeMode = Value
  val Resignation, CheckMate, Stalemate = Value
}