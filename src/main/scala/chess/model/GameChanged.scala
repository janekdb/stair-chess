package chess.model

import chess.model.GameOutcomeMode

sealed abstract class GameChanged

case class Won(colour: Colour, winMode: GameOutcomeMode) extends GameChanged
case class Drawn(drawMode: GameOutcomeMode)              extends GameChanged
