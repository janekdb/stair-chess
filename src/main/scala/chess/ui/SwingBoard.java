package chess.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;

public class SwingBoard extends JFrame implements Board {
	GridLayout boardLayout = new GridLayout(8, 8);

	public SwingBoard(String name) {
		super(name);
		setResizable(false);
	}

	private static final int BOARD_SIZE = 8;

	private static final int SQUARE_SIZE = 50;

	private static final Color WHITE_BACKGROUND = Color.WHITE;
	private static final Color BLACK_BACKGROUND = Color.LIGHT_GRAY;
	private static final Color[] BACKGROUNDS = new Color[] { WHITE_BACKGROUND, BLACK_BACKGROUND };
	private static final int SQUARE_BORDER_WIDTH = 0;

	private Color getBackgroundColor(final int col, final int row) {
		int s = (row - 1) * (BOARD_SIZE + 1) + (col - 1) + 1;
		return BACKGROUNDS[s % 2];
	}

	/** One-based */
	private int getIndex(final int col, final int row) {
		return (row - 1) * BOARD_SIZE + (col - 1);
	}

	private static class Square {
		JLabel label;
		String graphicName;

		Square(JLabel label) {
			if (label == null) {
				throw new IllegalArgumentException();
			}
			this.label = label;
		}
	}

	private Square[] squares;

	public void addComponentsToPane(final Container pane, final boolean interactiveMode) {
		final JPanel boardPanel = new JPanel();
		boardPanel.setLayout(boardLayout);

		int borderedSqSz = SQUARE_SIZE + 2 * SQUARE_BORDER_WIDTH;
		boardPanel.setPreferredSize(new Dimension(borderedSqSz * BOARD_SIZE, borderedSqSz * BOARD_SIZE));

		squares = new Square[BOARD_SIZE * BOARD_SIZE];

		for (int row = BOARD_SIZE; row >= 1; row--) {
			for (int col = 1; col <= BOARD_SIZE; col++) {
				Color bg = getBackgroundColor(col, row);
				JLabel lb = new JLabel();
				lb.setBackground(bg);
				lb.setOpaque(true);
				Border border = new LineBorder(Color.GRAY, SQUARE_BORDER_WIDTH);
				lb.setBorder(border);
				squares[getIndex(col, row)] = new Square(lb);

				boardPanel.add(lb);
			}
		}

		pane.add(boardPanel, BorderLayout.CENTER);
		if (interactiveMode) {
			moveEntry = new InputFieldMoveEntry();
			Container mec = moveEntry.getContainer();
			pane.add(mec, BorderLayout.SOUTH);
		}
	}

	private MoveEntry moveEntry;

	private final Map<String, Icon> icons = new HashMap<String, Icon>();

	private static final String ICON_PATH_TEMPLATE = "piece/%s.png";

	private Icon getPiece(final String imageName) {
		Icon icon = icons.get(imageName);
		if (icon != null) {
			return icon;
		}
		try {
			String imagePath = String.format(ICON_PATH_TEMPLATE, imageName);
			InputStream is = getClass().getClassLoader().getResourceAsStream(imagePath);
			BufferedImage myPicture = ImageIO.read(is);
			icon = new ImageIcon(myPicture);
		} catch (Exception e) {
			throw new RuntimeException("imageName: '" + imageName + "' : " + e);
		}
		icons.put(imageName, icon);
		return icon;
	}

	/**
	 * Create the GUI and show it. For thread safety, this method is invoked
	 * from the event dispatch thread.
	 */
	// Create and set up the window.
	private static SwingBoard createAndShowGUI(final boolean interactiveMode) {
		SwingBoard frame = new SwingBoard("Stair Chess");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Set up the content pane.
		frame.addComponentsToPane(frame.getContentPane(), interactiveMode);
		// Display the window.
		frame.pack();
		frame.setVisible(true);

		return frame;
	}

	/**
	 * @param interactiveMode
	 *            if true interactive element will be included.
	 * @return
	 */
	public static Board createAndShowBoard(final boolean interactiveMode) {
		/* Use an appropriate Look and Feel */
		try {
			// UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
			UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
		} catch (Exception ex) {
			throw new RuntimeException(ex);
		}

		/* Turn off metal's use of bold fonts */
		UIManager.put("swing.boldMetal", Boolean.FALSE);

		final CountDownLatch latch = new CountDownLatch(1);

		// Schedule a job for the event dispatch thread:
		// creating and showing this application's GUI.
		final SwingBoard[] boardHolder = new SwingBoard[1];
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				boardHolder[0] = createAndShowGUI(interactiveMode);
				latch.countDown();
			}
		});

		try {
			latch.await();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
		return boardHolder[0];
	}

	public static void main(String[] args) {
		Board board = createAndShowBoard(true);
	}

	public void clearSquare(final int col, final int row) {
		Square sq = squares[getIndex(col, row)];
		sq.label.setIcon(null);
		sq.graphicName = null;
	}

	/**
	 * @param piece
	 *            A piece category such as black-rook
	 */
	@SuppressWarnings("UnnecessaryLocalVariable")
	public void setPiece(final int col, final int row, final String piece) {
		String graphicName = piece;
		Square sq = squares[getIndex(col, row)];
		sq.label.setIcon(getPiece(graphicName));
		sq.graphicName = graphicName;
	}

	public void showWon(final String colour, final String wonMode) {
		final JPanel boardPanel = new JPanel();
		boardPanel.add(new JButton("Won by " + colour + " with " + wonMode));
		setTitle(String.format("Won by %s with %s", colour, wonMode));
	}

	public void showDrawn(final String drawMode) {
		setTitle(String.format("Drawn with %s", drawMode));
	}

	public void close() {
		super.dispose();
	}

	public void addMoveEntryListener(MoveEntryListener listener) {
		if (listener == null) {
			throw new IllegalArgumentException("listener was null");
		}
		moveEntry.addMoveEntryListener(listener);
	}

	/* Remove all user input from move entry interface */
	public void clearMoveEntry() {
		moveEntry.clearMoveEntry();
	}
}