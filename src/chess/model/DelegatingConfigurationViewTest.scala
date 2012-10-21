package chess.model
import chess.model.Colours.Black
import test.Main
import test.Test
import test.TestUtils

object DelegatingConfigurationViewTest extends Test with TestUtils with Main {

  def runTests {
    appliedResultsInModifedConfiguration
  }

  private def appliedResultsInModifedConfiguration {
    val conf = new GridConfiguration
    conf.add("e5", Black, Pawn())
    val del = new DelegatingConfigurationView(conf).applied(MovePiece("e5", "e6"))
    assertEquals(del.getExistingPiece("e6"), (Black, Pawn(), Some(new Position("e5"))))
  }
}