package chess.model

import GameOutcomeModes.GameOutcomeMode

abstract class GameChanged

case class Won(colour: Colour, winMode: GameOutcomeMode) extends GameChanged
case class Drawn(drawMode: GameOutcomeMode) extends GameChanged