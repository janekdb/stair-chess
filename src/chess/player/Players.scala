package chess.player
import chess.model.Colour
import chess.model.ConfigurationView
import chess.model.MoveExplorer
import chess.ranker.CheckMatingRanker
import chess.ranker.CapturingRanker
import chess.ranker.CaptureEvadingRanker

object Players {

  def checkMatingCapturingPlayer(colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val capturingRanker = new CapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, capturingRanker)
    new ShellPlayer("Checkmating, Capturing Player", colour, explorerFactory, ranker)
  }

  def capturingPlayer(colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val ranker = new CapturingRanker(explorerFactory, colour)
    new ShellPlayer("Capturing Player", colour, explorerFactory, ranker)
  }

  def capturingEvadingPlayer(colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val ranker = new CaptureEvadingRanker(explorerFactory, colour)
    new ShellPlayer("Capture Evading Player", colour, explorerFactory, ranker)
  }
}