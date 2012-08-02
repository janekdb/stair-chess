package chess.app

import chess.player.Player
import chess.model.Configuration

import test.{ Main, Test, TestUtils }

object ScoreCardTest extends Test with TestUtils with Main {

  private implicit def player2String(player: Player): String = player.getName

  def runTests {
    emptyScoreCardHasEmptyScores
    winsOrderedByMostFirst
    drawsOrderedByName
  }

  private class PlayerBase(name: String) extends Player {
    def getName = name
    def getMove(configuration: Configuration) = None
  }

  private val PLAYER_1 = new PlayerBase("player-1")
  private val PLAYER_2 = new PlayerBase("player-2")
  private val PLAYER_3 = new PlayerBase("player-3")
  private val PLAYER_4 = new PlayerBase("player-4")
  private val PLAYER_5 = new PlayerBase("player-5")

  private def emptyScoreCardHasEmptyScores {
    val sc = new ScoreCard(Set())
    assertEquals(List(), sc.getWins)
    assertEquals(List(), sc.getDraws)
  }

  private def winsOrderedByMostFirst {
    val sc = new ScoreCard(Set(PLAYER_1, PLAYER_2, PLAYER_3, PLAYER_4, PLAYER_5))
    sc.addWin(PLAYER_1, PLAYER_2)
    sc.addWin(PLAYER_1, PLAYER_4)
    sc.addWin(PLAYER_4, PLAYER_1)
    sc.addWin(PLAYER_2, PLAYER_3)
    /* No win for player 5 */
    sc.addDraw(PLAYER_1, PLAYER_5)
    val wins = sc.getWins
    assertEquals(List((PLAYER_1.getName, 2), (PLAYER_2.getName, 1), (PLAYER_4.getName, 1), (PLAYER_3.getName, 0), (PLAYER_5.getName, 0)), wins)
  }

  private def drawsOrderedByName {
    val sc = new ScoreCard(Set(PLAYER_1, PLAYER_2, PLAYER_3, PLAYER_4, PLAYER_5))
    sc.addDraw(PLAYER_1, PLAYER_2)
    sc.addDraw(PLAYER_1, PLAYER_4)
    sc.addDraw(PLAYER_2, PLAYER_3)
    /* No draw for player 5 */
    sc.addWin(PLAYER_1, PLAYER_5)
    val draws = sc.getDraws
    assertEquals(List((PLAYER_1.getName, 2), (PLAYER_2.getName, 2), (PLAYER_3.getName, 1), (PLAYER_4.getName, 1), (PLAYER_5.getName, 0)), draws)
  }

}