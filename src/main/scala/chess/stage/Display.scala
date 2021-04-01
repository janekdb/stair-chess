package chess.stage

/**
 * Display information related to the staged event which could be a tournament or a 1-1 match.
 */
object Display {

  def renderScoreCard(scoreCard: ScoreCard) {

    println("Scores:")
    println("Wins:")

    val maxNameWidth = scoreCard.players.foldLeft(0) { (i, name) => i max name.length }
    val pad = (s: String) => { s.padTo(maxNameWidth, ' ') }
    val printScore = (name: String, score: Int) => println(pad(name) + " : " + "%4d" format score)

    scoreCard.getWins.foreach {
      case (name, score) => printScore(name, score)
    }
    println("Draws:")
    scoreCard.getDraws.foreach {
      case (name, score) => printScore(name, score)
    }
  }

}