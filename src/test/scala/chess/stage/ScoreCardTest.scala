package chess.stage

import chess.model.{Configuration, Move}
import chess.player.Player
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import test.TestUtils

object ScoreCardTest extends AnyWordSpec with Matchers with TestUtils {

  private implicit def player2String(player: Player): String = player.getName

  "A ScoreCard" when {
    "scoring" should {
      "start empty" in {
        emptyScoreCardHasEmptyScores()
      }
      "order wins by most first" in {
        winsOrderedByMostFirst()
      }
      "order draws by name" in {
        drawsOrderedByName()
      }
      "order scores by name" in {
        scoresOrderedByName()
      }
    }
  }

  private class PlayerBase(name: String) extends Player {
    def getName: String = name

    def getMove(configuration: Configuration): Option[Move] = None
  }

  private val PLAYER_1 = new PlayerBase("player-1")
  private val PLAYER_2 = new PlayerBase("player-2")
  private val PLAYER_3 = new PlayerBase("player-3")
  private val PLAYER_4 = new PlayerBase("player-4")
  private val PLAYER_5 = new PlayerBase("player-5")

  private def emptyScoreCardHasEmptyScores(): Assertion = {
    val sc = new ScoreCard(Set())
    sc.getWins shouldBe empty
    sc.getDraws shouldBe empty
    sc.getScores shouldBe empty
  }

  private def winsOrderedByMostFirst(): Assertion = {
    val sc = new ScoreCard(Set(PLAYER_1, PLAYER_2, PLAYER_3, PLAYER_4, PLAYER_5))
    sc.addWin(PLAYER_1, PLAYER_2)
    sc.addWin(PLAYER_1, PLAYER_4)
    sc.addWin(PLAYER_4, PLAYER_1)
    sc.addWin(PLAYER_2, PLAYER_3)
    /* No win for player 5 */
    sc.addDraw(PLAYER_1, PLAYER_5)
    val wins = sc.getWins
    wins shouldBe
      (PLAYER_1.getName, 2) :: (PLAYER_2.getName, 1) :: (PLAYER_4.getName, 1) :: (PLAYER_3.getName, 0) :: (PLAYER_5.getName, 0) :: Nil
  }

  private def drawsOrderedByName(): Assertion = {
    val sc = new ScoreCard(Set(PLAYER_1, PLAYER_2, PLAYER_3, PLAYER_4, PLAYER_5))
    sc.addDraw(PLAYER_1, PLAYER_2)
    sc.addDraw(PLAYER_1, PLAYER_4)
    sc.addDraw(PLAYER_2, PLAYER_3)
    /* No draw for player 5 */
    sc.addWin(PLAYER_1, PLAYER_5)
    val draws = sc.getDraws
    draws shouldBe
      (PLAYER_1.getName, 2) :: (PLAYER_2.getName, 2) :: (PLAYER_3.getName, 1) :: (PLAYER_4.getName, 1) :: (PLAYER_5.getName, 0) :: Nil
  }

  private def scoresOrderedByName(): Assertion = {
    val sc = new ScoreCard(Set(PLAYER_1, PLAYER_2, PLAYER_3, PLAYER_4, PLAYER_5))
    sc.addDraw(PLAYER_1, PLAYER_2)
    sc.addDraw(PLAYER_1, PLAYER_2)
    sc.addDraw(PLAYER_2, PLAYER_3)
    sc.addWin(PLAYER_1, PLAYER_5)
    sc.addWin(PLAYER_3, PLAYER_5)
    val scores = sc.getScores
    scores shouldBe
      Scores(PLAYER_1, 1, 2, 0) ::
        Scores(PLAYER_2, 0, 3, 0) ::
        Scores(PLAYER_3, 1, 1, 0) ::
        Scores(PLAYER_4, 0, 0, 0) ::
        Scores(PLAYER_5, 0, 0, 2) ::
        Nil
  }

}
