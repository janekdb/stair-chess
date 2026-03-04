# New Move Selection Strategies

Three new `MoveRanker` implementations grounded in chess literature, designed to complement the existing tactical rankers with positional, dynamic, and defensive dimensions.

---

## 1. CenterControlRanker

### Chess Theory Basis

Control of the center is one of the oldest and most fundamental strategic principles, championed by Wilhelm Steinitz (first World Champion) and formalized by Siegbert Tarrasch. The key insight is that pieces placed in or directed toward the center squares (d4, d5, e4, e5) exert influence over more of the board. Nimzowitsch's *My System* (1925) distinguishes between direct occupation and indirect control, but agrees on the center's primacy. Modern engines like Stockfish still encode center-control bonuses in their piece-square tables.

### Implementation Sketch

Assign each square a positional weight based on distance from center. For each candidate move, score it by the weight of the destination square. Moves to the four central squares (d4/d5/e4/e5) rank highest; the extended center (c3-f6 ring) ranks next; edges and corners rank lowest.

```scala
class CenterControlRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour)
    extends MoveRanker {

  // Weight: 3 for d4/d5/e4/e5, 2 for extended center, 1 for third ring, 0 for edges
  private def centralityScore(pos: Position): Int = {
    val colDist = Math.abs(pos.getCol - 4.5)
    val rowDist = Math.abs(pos.getRow - 4.5)
    val maxDist = Math.max(colDist, rowDist)
    (4 - maxDist.toInt)
  }

  private def rank(move: Move): Int = move match {
    case m: SimpleMove => centralityScore(m.end)
    case _: Castle     => 1
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] =
    ranker.rankAsList(moves, rank)
}
```

### Why It Complements Existing Rankers

No current ranker considers *where* pieces end up on the board. All existing rankers evaluate tactical consequences (captures, checks, threats). CenterControlRanker adds a purely positional dimension. Chained as a low-priority tiebreaker (e.g., `ChainedMoveRanker(CheckMatingRanker, CaptureEvadingRanker, CenterControlRanker)`), it would break ties among otherwise-equal moves by preferring central placement rather than random selection within the top tier.

---

## 2. MobilityRanker

### Chess Theory Basis

Maximizing piece mobility was identified by Claude Shannon in his foundational 1950 paper *"Programming a Computer for Playing Chess"* as one of the key evaluation factors. Shannon's evaluation function explicitly included a term for number of legal moves. Nimzowitsch's concept of *Beweglichkeit* (mobility) in *My System* argues that the side with more available moves has a strategic advantage -- more options create more threats and more flexibility. Every serious chess engine from the 1960s to today includes a mobility component in its evaluation.

### Implementation Sketch

For each candidate move, apply it to get the resulting board state, then count how many legal moves the player has in that position. Rank moves that lead to more options higher.

```scala
class MobilityRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour)
    extends MoveRanker {

  private def rank(conf: ConfigurationView)(move: Move): Int = {
    val future = conf.applied(move)
    val explorer = explorerFactory(future)
    explorer.legalMoves(colour).size
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] =
    ranker.rankAsList(moves, rank(conf))
}
```

### Why It Complements Existing Rankers

`CaptureEvadingRanker` is superficially similar but only counts the *opponent's* capturing moves -- a purely defensive metric. `MobilityRanker` measures the player's *own* total options, which is an offensive/dynamic metric. A position where you have 35 legal moves vs. 15 is qualitatively different even if the opponent's capture count is the same. This ranker would naturally prefer developing pieces off the back rank (since developed pieces generate more moves), avoid self-pins, and favor open positions -- all recognized as good chess strategy.

---

## 3. KingSafetyRanker

### Chess Theory Basis

Vladimir Vukovic's *The Art of Attack in Chess* (1965) is the definitive work on king attacks, establishing that king safety is one of the three most important evaluation factors (alongside material and pawn structure). The core principle: an exposed king is a strategic liability that outweighs material advantages. Modern engines devote substantial evaluation weight to king safety, measuring pawn shelter, open files near the king, and attacker proximity. Kasparov's playing style famously exploited opponents' king-safety weaknesses.

### Implementation Sketch

Evaluate king safety by two factors: (a) prioritize castling when available (gets the king behind pawns), and (b) count how many opponent pieces can reach squares adjacent to the king after the move. Fewer attackers near the king = safer = higher rank.

```scala
class KingSafetyRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour)
    extends MoveRanker {

  private def rank(conf: ConfigurationView)(move: Move): Int = move match {
    case _: Castle => 100

    case _ =>
      val future = conf.applied(move)
      val explorer = explorerFactory(future)
      val opponentMoves = explorer.legalMoves(colour.opposite)
      val kingPos = future.locatePieces(colour, King).head

      val kingZone = for {
        dc <- -1 to 1; dr <- -1 to 1
        if kingPos.canOffset(dc, dr)
      } yield kingPos.offset(dc, dr)

      val attacksNearKing = opponentMoves.count {
        case m: SimpleMove => kingZone.contains(m.end)
        case _             => false
      }

      -attacksNearKing
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] =
    ranker.rankAsList(moves, rank(conf))
}
```

### Why It Complements Existing Rankers

No existing ranker considers king safety. `CaptureEvadingRanker` minimizes the opponent's total captures but doesn't distinguish between a pawn being threatened in a corner vs. the king's position being compromised. `KingSafetyRanker` specifically evaluates the king's vulnerability -- a rook aimed at the king's file is far more dangerous than one threatening a distant pawn. It also uniquely prioritizes castling, which no current ranker does.

---

## Summary

| Ranker | Chess Principle | Key Literature | Metric |
|--------|----------------|----------------|--------|
| **CenterControlRanker** | Central occupation | Steinitz, Tarrasch, Nimzowitsch *My System* | Destination square centrality |
| **MobilityRanker** | Piece activity | Shannon (1950), Nimzowitsch *Beweglichkeit* | Player's legal move count after move |
| **KingSafetyRanker** | King protection | Vukovic *Art of Attack in Chess*, Kasparov | Opponent attacks near king + castling bonus |

These three cover **positional** (center), **dynamic** (mobility), and **defensive** (king safety) dimensions -- all absent from the current tactical-only ranker set. A strong composite player might chain them as:

```scala
ChainedMoveRanker(CheckMatingRanker, KingSafetyRanker, CaptureEvadingRanker,
                  MobilityRanker, HighValueCapturingRanker, CenterControlRanker)
```
