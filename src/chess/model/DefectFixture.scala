package chess.model

import Colours.Black

object DefectFixture {

  // TODO: Use TestUtils instead of duplicating implicit functions
  implicit def string2MovePiece(s: String) = new MovePiece(s)
  implicit def string2Position(s: String) = new Position(s)

  var defect5Moves = List[Move]()
  defect5Moves ::= new MovePiece("g2g4")
  defect5Moves ::= new MovePiece("f7f5")
  defect5Moves ::= new MovePiece("f2f3")
  defect5Moves ::= new MovePiece("b8c6")
  defect5Moves ::= new MovePiece("c2c3")
  defect5Moves ::= new MovePiece("d7d6")
  defect5Moves ::= new MovePiece("a2a3")
  defect5Moves ::= new MovePiece("g7g5")
  defect5Moves ::= new MovePiece("a3a4")
  defect5Moves ::= new MovePiece("f5f4")
  defect5Moves ::= new MovePiece("h2h4")
  defect5Moves ::= new MovePiece("c6b8")
  defect5Moves ::= new MovePiece("e2e4")
  defect5Moves ::= new MovePiece("b8a6")
  defect5Moves ::= new MovePiece("f1b5")
  defect5Moves ::= new MovePiece("c7c6")
  defect5Moves ::= new MovePieceCapturing("b5", "c6")
  defect5Moves ::= new MovePiece("c8d7")
  defect5Moves ::= new MovePieceCapturing("c6", "d7")
  defect5Moves ::= new MovePiece("e8f7")
  defect5Moves ::= new MovePiece("d7e6")
  defect5Moves ::= new MovePiece("f7e8")
  defect5Moves ::= new MovePiece("e6f7")
  defect5Moves ::= new MovePiece("e8d7")
  defect5Moves ::= new MovePiece("f7e8")
  defect5Moves ::= new MovePiece("d7c8")
  defect5Moves ::= new MovePiece("e8d7")
  defect5Moves ::= new MovePiece("c8b8")
  defect5Moves ::= new MovePiece("d7c8")
  defect5Moves ::= new MovePiece("b8c7")
  defect5Moves ::= new MovePieceCapturing("c8", "b7")
  defect5Moves ::= new MovePieceCapturing("g5", "h4")
  defect5Moves ::= new MovePiece("b2b3")
  defect5Moves ::= new MovePiece("d8b8")
  defect5Moves ::= new MovePiece("b7d5")
  defect5Moves ::= new MovePiece("b8e8")
  defect5Moves ::= new MovePieceCapturing("d5", "g8")
  defect5Moves ::= new MovePiece("h7h6")
  defect5Moves ::= new MovePiece("c3c4")
  defect5Moves ::= new MovePiece("c7b6")
  defect5Moves ::= new MovePiece("c4c5")
  defect5Moves ::= new MovePieceCapturing("d6", "c5")
  defect5Moves ::= new MovePiece("a4a5")
  defect5Moves = defect5Moves.reverse

  /* This move should be rejected after playing the defect5Moves moves */
  val defect5FinalMove = new Castle(Black, Long)
}