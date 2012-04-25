package chess.ui;


public interface Board {

	/** 1-based */
	void clearLabel(final int col, final int row);

	/** 1-based */
	void setLabel(final int col, final int row, final String label);

	// TODO: Remove this method once the Board has access to a Configuration
	/** 1-based */
	String getLabel(final int col, final int row);

}
