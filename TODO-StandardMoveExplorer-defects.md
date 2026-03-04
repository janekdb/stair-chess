# StandardMoveExplorer Defects

## Defect 1: Pawn can "capture" by double-advancing forward (Bug)

**File**: `StandardMoveExplorer.scala:106-115`
**Severity**: High

`pawnMoveAllowed` for `pawnForwardTwo` checks that the **intermediate** square is empty, but never checks the **destination** square. After `pawnMoveAllowed` returns true, `getEndPositions` (line 70-72) adds the destination to `basicPositions` even if an opponent piece is there. Then `generateMoves` creates a `MovePieceCapturing`, and `rejectIllegalMove` accepts it since the position IS in `basicPositions`.

**Example**: White pawn at e2, black piece at e4, e3 empty → `MovePieceCapturing(e2, e4)` is generated and accepted. Pawns cannot capture forward in chess.

**Fix**: Add `&& !conf.exists(startPosition.offset(0, dRow))` to the `pawnForwardTwo` check, mirroring what `pawnForward` does:

```scala
// current (line 112-114):
val r = if (dRow == 2) 1 else -1
val p = startPosition.offset(0, r)
!conf.exists(p)

// should also check destination:
val r = if (dRow == 2) 1 else -1
val intermediate = startPosition.offset(0, r)
val destination = startPosition.offset(0, dRow)
!conf.exists(intermediate) && !conf.exists(destination)
```

## Defect 2: Pawn double-advance allowed from any row (Bug)

**File**: `StandardMoveExplorer.scala:107-109`
**Severity**: High

`pawnForwardTwo` uses `previousPosition` (whether the pawn has ever moved) as a proxy for "pawn is on starting row." A pawn placed via `conf.add()` gets `previousPosition = None`, so even a pawn placed on row 5 can double-advance.

This is the root cause of the bug noted in the test comment — `MovePiece(c5, c7)` is generated for a white pawn placed at c5 because it was never "moved" despite not being on row 2.

In normal gameplay this doesn't manifest (pawns always start on row 2/7), but it means the move explorer is not self-contained — it relies on external invariants.

**Fix**: Also check the starting row:

```scala
case Placed(colour, _, Some(_)) => false
case Placed(colour, _, None) =>
  val startingRow = if (colour.pawnRowIncrement == 1) 2 else 7
  if (startPosition.getRow != startingRow) false
  else {
    val r = if (dRow == 2) 1 else -1
    val p = startPosition.offset(0, r)
    !conf.exists(p)
  }
```

## Defect 3: Promotion limited to Knight and Queen (Incorrect rules)

**File**: `StandardMoveExplorer.scala:327`
**Severity**: Medium

```scala
private val promotionPieces = List(Knight, Queen)
```

Chess rules allow promotion to **any** piece except King and Pawn — Bishop and Rook promotions are missing. Under-promotion to Rook or Bishop is sometimes the only move that avoids stalemate.

## Defect 4: `kingInCheck` uses `getBasicPositions` — fragile for pawn attack detection

**File**: `StandardMoveExplorer.scala:270`
**Severity**: Low

```scala
opponentPositions.exists(p => getBasicPositions(p) contains king)
```

`getBasicPositions` uses `pawnMoveAllowed`, which only includes pawn diagonal squares when a piece is present there. This works for check detection *only because the king is on the square*, making `conf.exists(p)` return true. But it also means:

- Forward pawn moves (non-attacking) are included in `getBasicPositions`, which is semantically wrong for "is this square attacked?"
- The correctness depends on the subtle interaction between `pawnMoveAllowed` and the king being present

Compare with castling validation (line 240) which correctly uses `getAttackedPositions`. Using `getAttackedPositions` in `kingInCheck` would be more correct and robust:

```scala
opponentPositions.exists(p => getAttackedPositions(p) contains king)
```

## Defect 5: `getBasicPositions` and `testPieceColour` have unnecessarily wide visibility

**File**: `StandardMoveExplorer.scala:30, 142`
**Severity**: Low

Both are `def` (public) but appear to be internal implementation details. `getBasicPositions` is used externally only in tests; `testPieceColour` likewise. These should be `private` (or `private[model]` if test access is needed).
