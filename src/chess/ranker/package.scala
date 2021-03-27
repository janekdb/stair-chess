import chess.model.Move

package object ranker {

  /**
   * Given a map move lists calculate a linear representation where the most
   * favoured move list is at the head of the list.
   */
  private def convertRankingMapToList(map: Map[Int, List[Move]]): List[List[Move]] =
    map.toList.sortBy(_._1).map(_._2)

  // TODO: Allow ranking maps to use Ordered for keys instead of Int
  // TODO: Add test for this method
  /**
   * @param moves The list of moves to rank
   * @param rank A function that assigns a value to a move where a higher value equates to a preferred move
   * @return The ranked moves
   */
  def rankAsList(moves: List[Move], rank: Move => Int): List[List[Move]] =
    convertRankingMapToList(moves.groupBy(rank)).reverse
}