Code Review: stair-chess

Critical Bugs

1. HighValueCapturingRanker only handles MovePieceCapturing, missing other capture types
   src/main/scala/chess/ranker/HighValueCapturingRanker.scala:12 — Matches on MovePieceCapturing only, but Capturing is also mixed into EnPassant and PromoteCapturing. These fall through to RANKING_LOW, so en passant and
   promote-captures are never ranked by piece value. Compare with CapturingRanker which correctly matches on _: Capturing.

2. MoveTest assertions are broken — message content is never verified
   src/test/scala/chess/model/MoveTest.scala:20-21, 28-29 — The code reads:
   caught.getMessage should contain
   "different"
   These are two separate statements. The should contain has no argument, and "different" is a discarded string literal. The intended code was likely should include("different"). The tests pass for the wrong reason.

3. Computer, Human, DumbPlayer crash with MatchError on empty move list
   src/main/scala/chess/player/Computer.scala:15, Human.scala:16, DumbPlayer.scala:11 — All use val m :: ms = moves which throws MatchError when moves is Nil. Once scripted moves are consumed, any further call crashes.

4. RandomPlayer ignores the current board state
   src/main/scala/chess/player/RandomPlayer.scala:9,14 — Takes a single MoveExplorer (not a factory) and ignores the configuration parameter to getMove. It always picks from whatever configuration the explorer was initially constructed
   with, not the current game state.

5. Four test files declared as object instead of class — may not be discovered by ScalaTest
   GridConfigurationTest, DelegatingConfigurationViewTest, ChainedMoveRankerTest, ScoreCardTest are all object extends AnyWordSpec. ScalaTest expects class. These tests may silently not run.

6. Promote/PromoteCapturing validation runs only in secondary constructor
   src/main/scala/chess/model/Move.scala:62-65, 71-74 — rejectInvalidPromotionPiece is called after this(...) in auxiliary constructors only. The primary constructor Promote(position, King) bypasses validation entirely.

 ---
Significant Bugs

7. FIXED: Dead computation in Pawn.movements
   src/main/scala/chess/model/Piece.scala:15 — First map call's result is discarded; only the second map on line 16 is returned. Harmless but wasteful on every call.

8. FIXED: Position.hashCode has extreme collisions
   src/main/scala/chess/model/Position.scala:40 — row + col yields only 15 distinct values for 64 positions. Degrades Map performance since Position is a key in GridConfiguration.pieces. Should be row * 8 + col or similar.

9. getWinner throws on None
   src/main/scala/chess/model/BoardModel.scala:78 — gameOutcome.get.winner will throw NoSuchElementException when gameOutcome is None, despite the Option[Colour] return type suggesting safety.

10. Division by zero in Display.renderScoreCard
    src/main/scala/chess/stage/Display.scala:15 — 100f * win / total where total is 0 for players who haven't played yet. Produces NaN/Infinity in tournament output.

11. Swing thread-safety violations in SwingBoard
    src/main/scala/chess/ui/SwingBoard.java:178-203 — clearSquare, setPiece, showWon, showDrawn modify Swing components from the game thread, not the EDT. Can cause visual artifacts or crashes.

12. Resigned crashes the application
    src/main/scala/chess/ui/BoardAdapter.scala:59-60 — throw new RuntimeException instead of handling the event.

13. ForkingRanker is a no-op stub
    src/main/scala/chess/ranker/ForkingRanker.scala:8 — Returns all moves in a single tier (no ranking). Its test explicitly calls fail().

---
Missing Chess Rules

14. Promotion only generates Knight and Queen
    src/main/scala/chess/model/StandardMoveExplorer.scala:328 — promotionPieces = List(Knight, Queen). Missing Bishop and Rook under-promotions which are legal and occasionally critical.

15. No automatic stalemate detection
    src/main/scala/chess/model/BoardModel.scala:118-129 — After applying a move, checkmate is checked but stalemate is not. Stalemate only triggers when the caller explicitly passes None.

16. No draw by insufficient material, threefold repetition, or fifty-move rule — All acknowledged as TODOs.

---
Design Issues

17. Move and BoardChanged are not sealed
    Move.scala:30, BoardChanged.scala:3 — Prevents exhaustive match warnings. Every handler has fragile wildcard defaults that throw at runtime.

18. Player.getMove takes mutable Configuration instead of ConfigurationView
    src/main/scala/chess/player/Player.scala:8 — Players only need read access but receive full mutation capability.

19. Exception-based control flow for legal move filtering
    src/main/scala/chess/model/StandardMoveExplorer.scala:344-352 — moveAcceptable catches IllegalMoveException for every candidate move. Expensive and unidiomatic; should return a validation result.

20. ChainedMoveRanker lives in player package instead of ranker
    src/main/scala/chess/player/ChainedMoveRanker.scala:1 — It extends MoveRanker and is a pure ranking combinator, belonging conceptually in chess.ranker.

21. CapturingRanker accepts explorerFactory and colour but never uses them
    src/main/scala/chess/ranker/CapturingRanker.scala:12 — Constructor parameters exist only for consistency with other rankers but add false dependencies.

22. CaptureEvadingRanker counts capturing moves, not threatened pieces
    src/main/scala/chess/ranker/CaptureEvadingRanker.scala:17-25 — Three moves targeting one pawn ranks worse than one move targeting a queen. Scaladoc says "reduces attacked pieces" but code counts moves.

---
Code Smells

23. Pervasive public mutability — GridConfiguration.pieces is a public var (GridConfiguration.scala:5), BoardModel subscriber lists are public var constructor params (:31-32), ScoreCard maps are public mutable HashMaps (:9-11),
    PlayerSelector fields are public var (:5).

24. null returned from GridConfiguration.getRows
    src/main/scala/chess/model/GridConfiguration.scala:56 — Forces downstream null checks in ConfigurationView.getTextRepresentation. Should use Option.

25. Computer/Human/DumbPlayer are near-identical
    Three classes share the same pop-from-mutable-list logic with only different hardcoded move sequences.

26. Players.scala has 9 boilerplate factory methods
    src/main/scala/chess/player/Players.scala:14-106 — All follow the same create-rankers-chain-wrap pattern. A builder or List[MoveRanker]-based factory would eliminate the duplication.

27. DefectFixture is test data in src/main
    src/main/scala/chess/model/DefectFixture.scala — Should be in src/test.

28. Misc is an empty object
    src/main/scala/chess/model/Misc.scala — Dead code, should be deleted.

29. wonGuard() is defined but never called
    src/main/scala/chess/model/BoardModel.scala:67 — Dead code.

30. Non-standard piece values
    src/main/scala/chess/model/Piece.scala:19-45 — Pawn=1, Knight=2, Bishop=3, Rook=4, Queen=5, King=6. Standard values are ~1/3/3/5/9.


---
Test Issues

31. Mega-test anti-pattern — BoardModelTest, StandardMoveExplorerTest (30+ sub-methods in one in block), StandardMoveParserTest, ChainedMoveRankerTest, GridConfigurationTest all cram many independent scenarios into single test cases.
    If an early method fails, all subsequent tests are silently skipped.

32. RandomPlayerTest.isRandom doesn't prove randomness
    src/test/scala/chess/player/RandomPlayerTest.scala:33-41 — Applies m1 to the board before getting m2, so even a deterministic player would produce different moves.

33. Duplicated explorerFactory lambda — Identical (cv: ConfigurationView) => new StandardMoveExplorer(cv) appears in 7+ test files. Should be extracted to TestUtils.

34. Missing test coverage — No tests for: promotion through BoardModel, short castling through BoardModel, black en passant through BoardModel, game-completion guard, ConfigurationView.getTextRepresentation.

35. Unused render method
    src/test/scala/chess/model/StandardMoveExplorerTest.scala:82-86 — Debugging leftover, never called.

---
Inconsistency

36. Three different patterns for handling unrecognized BoardChanged events — DelayingSubscriber uses assert(false), BoardAdapter uses TODO.throwRuntimeEx, TextUI uses throw new AssertionError. None use the existing
    UnhandledCaseException.

37. FIXED: & (eager) instead of && (short-circuit) in Position
    src/main/scala/chess/model/Position.scala:24,27 — Functionally correct but non-idiomatic.

38. Mixed Java/Scala naming conventions — getRows, getCol, getExistingPiece etc. follow Java getter style; idiomatic Scala would drop the get prefix.

