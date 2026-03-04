# Scala 2 → 3 Idiomatic Migration Plan

## Context

The project already compiles on Scala 3.8.1 and previous commits have addressed wildcard imports (`.*`), postfix syntax, eta-expansions, refutable pattern matches, and one enum conversion (`GameOutcomeMode`). What remains is converting Scala 2 idioms to their Scala 3 equivalents: `implicit def` → `given Conversion`, `implicit class` → `extension`, `package object` → top-level definitions, `new Object with Trait` → `new Trait`, and sealed hierarchies → `enum`.

Each step is an atomic commit that compiles and passes tests.

---

## Step 1: Fix test `object` declarations → `class`

ScalaTest may not discover tests declared as `object`. Convert to `class`.

**Files:**
- `src/test/scala/chess/model/GridConfigurationTest.scala` — `object` → `class`
- `src/test/scala/chess/model/DelegatingConfigurationViewTest.scala` — `object` → `class`
- `src/test/scala/chess/stage/ScoreCardTest.scala` — `object` → `class`
- `src/test/scala/chess/player/ChainedMoveRankerTest.scala` — `object` → `class`

## Step 2: Replace `new Object with Trait` → `new Trait`

**Files (9 sites):**
- `src/main/scala/chess/stage/ScoreCard.scala:36,50` — `new Object with Ordering[...]` → `new Ordering[...] { ... }`
- `src/test/scala/chess/model/BoardModelTest.scala:256,409` — `new Object with BoardChangedSubscriber/ConfigurationChangedSubscriber`
- `src/test/scala/chess/player/ChainedMoveRankerTest.scala:145,151,186` — `new Object with MoveRanker`
- `src/test/scala/chess/player/ShellPlayerTest.scala:31` — `new Object with MoveRanker`

## Step 3: Package object → top-level definitions

Convert `package object ranker` to top-level definitions in the same file.

**File:** `src/main/scala/chess/ranker/package.scala`

```scala
// Before
package chess
package object ranker {
  private def convertRankingMapToList(...) = ...
  def rankAsList(...) = ...
}

// After
package chess.ranker
private def convertRankingMapToList(...) = ...
def rankAsList(...) = ...
```

No other files need changes — callers already import `chess.ranker.rankAsList` or use it unqualified within the package.

## Step 4: `implicit class` → extension method

**File:** `src/test/scala/chess/model/MoveTest.scala`

```scala
// Before
private implicit class PositionHelper(val sc: StringContext) {
  def p(args: Any*): Position = new Position(sc.parts.head)
}

// After
extension (sc: StringContext)
  private def p(args: Any*): Position = new Position(sc.parts.head)
```

## Step 5: Main source `implicit def` → `given Conversion` or explicit calls

**Files and approach:**

| File | Implicit | Approach |
|------|----------|----------|
| `Library.scala:22` | `stringToMovePiece` | `given Conversion[String, MovePiece]` |
| `DefectFixture.scala:8-9` | `string2MovePiece`, `string2Position` | `given Conversion[String, MovePiece]` + `given Conversion[String, Position]` |
| `Computer.scala:8` | `stringToMovePiece` | `given Conversion[String, MovePiece]` |
| `Human.scala:9` | `stringToMovePiece` | `given Conversion[String, MovePiece]` |
| `StandardMoveExplorer.scala:21` | `tuple2list` | **Replace with explicit** `List(t._1, t._2)` at the single call site (line 222) — an implicit conversion from tuple to list is confusing |
| `BoardModel.scala:147` | `tuple2colour` (local) | **Remove** — it's only used inside `extractColour` but never actually called implicitly (all match cases already return `Colour` directly) |

## Step 6: Test source `implicit def` → `given Conversion`

**Files:**

| File | Implicits | Approach |
|------|-----------|----------|
| `TestUtils.scala:11-15` | 5 conversions | Convert each to `given Conversion[A, B]` |
| `BoardModelTest.scala:54` | `placementBuilder2List` | `given Conversion[PlacementsBuilder, List[...]]` |
| `StandardMoveParserTest.scala:13` | `iterableToList` | `given Conversion[Iterable[Move], List[Move]]` |
| `RandomPlayerTest.scala:12` | `optMove2Move` | `given Conversion[Option[Move], Move]` |
| `ScoreCardTest.scala:12` | `player2String` | `given Conversion[Player, String]` |

Example transformation for TestUtils:
```scala
// Before
implicit def string2Position(s: String): Position = new Position(s)

// After
given Conversion[String, Position] = new Position(_)
```

## Step 7: `Colour` sealed trait → Scala 3 enum

**Primary file:** `src/main/scala/chess/model/Colours.scala`

```scala
// Before
sealed trait Colour { def opposite: Colour; def homeRow: Int; ... }
object Colours {
  case object White extends Colour { ... }
  case object Black extends Colour { ... }
}

// After
enum Colour {
  case White, Black

  def opposite: Colour = this match
    case White => Black
    case Black => White

  def homeRow: Int = this match
    case White => Constants.WHITE_HOME_ROW
    case Black => Constants.BLACK_HOME_ROW

  def pawnRowIncrement: Int = this match
    case White => 1
    case Black => -1

  def enPassantRow: Int = this match
    case White => 5
    case Black => 4
}
```

**Import changes (~20 files):** Replace `import chess.model.Colours.{White, Black}` / `import chess.model.Colours.White` with `import chess.model.Colour.{White, Black}` / `import chess.model.Colour.White`. Delete the `Colours` wrapper object.

---

## Verification

After each step:
1. `sbt compile` — must succeed
2. `sbt test` — all tests must pass
3. `sbt scalafmt` — format code

After all steps:
- `sbt test` — full regression
- Verify no remaining `implicit def` in codebase (except any intentionally kept)
- Verify no remaining `package object` declarations
- Verify no remaining `new Object with` patterns
