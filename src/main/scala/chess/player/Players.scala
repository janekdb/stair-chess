package chess.player
import chess.model.Colour
import chess.model.ConfigurationView
import chess.model.MoveExplorer
import chess.ranker.{CaptureEvadingRanker, CapturingRanker, CheckMatingRanker, CheckingRanker, HighValueCapturingRanker}

object Players {

  def checkingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val ranker = new CheckingRanker(explorerFactory, colour)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCapturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val capturingRanker = new CapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, capturingRanker)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCaptureEvadingCapturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val captureEvadingRanker = new CaptureEvadingRanker(explorerFactory, colour)
    val capturingRanker = new CapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, captureEvadingRanker, capturingRanker)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCaptureEvadingHighValueCapturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val captureEvadingRanker = new CaptureEvadingRanker(explorerFactory, colour)
    val capturingRanker = new HighValueCapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, captureEvadingRanker, capturingRanker)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingHighValueCapturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val capturingRanker = new HighValueCapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, capturingRanker)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCheckingHighValueCapturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val checkingRanker = new CheckingRanker(explorerFactory, colour)
    val capturingRanker = new HighValueCapturingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, checkingRanker, capturingRanker)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def capturingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val ranker = new CapturingRanker(explorerFactory, colour)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCaptureEvadingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val cer = new CaptureEvadingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, cer)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }

  def checkMatingCaptureEvadingCheckingPlayer(name: String, colour: Colour, explorerFactory: ConfigurationView => MoveExplorer): Player = {
    val checkMatingRanker = new CheckMatingRanker(explorerFactory, colour)
    val cer = new CaptureEvadingRanker(explorerFactory, colour)
    val capr = new CheckingRanker(explorerFactory, colour)
    val ranker = new ChainedMoveRanker(checkMatingRanker, cer, capr)
    new ShellPlayer(name, colour, explorerFactory, ranker)
  }
}