package chess.model

import chess.model.GameOutcomeMode

enum GameChanged:
  case Won(colour: Colour, winMode: GameOutcomeMode)
  case Drawn(drowMode: GameOutcomeMode)