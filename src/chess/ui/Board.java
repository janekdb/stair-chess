package chess.ui;

// TODO: Consider removing this interface and using just BoardChangedSubscriber
public interface Board {

	/** 1-based */
	void clearSquare(final int col, final int row);

	/** 1-based */
	void setPiece(final int col, final int row, final String piece);

	// TODO: Remove this method once the Board has access to a read only Configuration
	/** 1-based */
	String getPiece(final int col, final int row);

	void showWon(final String colour, final String wonMode);
}
