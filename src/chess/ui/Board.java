package chess.ui;

public interface Board {

	/** 1-based */
	void clearSquare(final int col, final int row);

	/** 1-based */
	void setPiece(final int col, final int row, final String piece);

	void showWon(final String colour, final String wonMode);

	void showDrawn(final String drawnMode);

	/** Close the UI */
	void close();
}
