package chess.player
import chess.model.Colour
import chess.model.ConfigurationView
import chess.model.MoveExplorer
import chess.ranker.CheckMatingRanker
import chess.ranker.CapturingRanker
import chess.ranker.CaptureEvadingRanker
import chess.ranker.CheckingRanker

object Players {

  def checkingPlayer(colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val ranker = new CheckingRanker(explorerFactory, colour)
    new ShellPlayer("Checking Player", colour, explorerFactory, ranker)
  }

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

  def checkMatingCaptureEvadingPlayer(colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val cer = new CaptureEvadingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, cer)
    new ShellPlayer("Checkmating, Capture Evading Player", colour, explorerFactory, ranker)
  }
}