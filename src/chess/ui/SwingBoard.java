package chess.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.image.BufferedImage;
import java.io.File;
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

// TODO: Convert to Scala
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
	private static final Color[] BACKGROUNDS = new Color[] { WHITE_BACKGROUND,
			BLACK_BACKGROUND };
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

		Square(JLabel label, String graphicName) {
			if (label == null) {
				throw new IllegalArgumentException();
			}
			this.label = label;
			this.graphicName = graphicName;
		}
	}

	private Square[] squares;

	public void addComponentsToPane(final Container pane) {
		final JPanel boardPanel = new JPanel();
		boardPanel.setLayout(boardLayout);

		int borderedSqSz = SQUARE_SIZE + 2 * SQUARE_BORDER_WIDTH;
		boardPanel.setPreferredSize(new Dimension(borderedSqSz * BOARD_SIZE,
				borderedSqSz * BOARD_SIZE));

		squares = new Square[BOARD_SIZE * BOARD_SIZE];

		for (int row = BOARD_SIZE; row >= 1; row--) {
			for (int col = 1; col <= BOARD_SIZE; col++) {
				Color bg = getBackgroundColor(col, row);
				JLabel lb = new JLabel();
				lb.setBackground(bg);
				lb.setOpaque(true);
				Border border = new LineBorder(Color.GRAY, SQUARE_BORDER_WIDTH);
				lb.setBorder(border);
				squares[getIndex(col, row)] = new Square(lb, null);

				boardPanel.add(lb);
			}
		}

		pane.add(boardPanel, BorderLayout.CENTER);
	}

	// TODO: Convert to getResource method
	private Icon getPiece(final String imageName) {
		try {
			String PATH_TEMPLATE = "C:\\Users\\jdb\\workspaces\\main\\stair-chess\\src\\chess\\resource\\piece\\%s.png";
			String imagePath = String.format(PATH_TEMPLATE, imageName);
			BufferedImage myPicture = ImageIO.read(new File(imagePath));
			return new ImageIcon(myPicture);
		} catch (Exception e) {
			throw new RuntimeException("imageName: '" + imageName +"' : " + e);
		}
	}

	/**
	 * Create the GUI and show it. For thread safety, this method is invoked
	 * from the event dispatch thread.
	 */
	private static SwingBoard createAndShowGUI() {
		// Create and set up the window.
		SwingBoard frame = new SwingBoard("Stair Chess");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Set up the content pane.
		frame.addComponentsToPane(frame.getContentPane());
		// Display the window.
		frame.pack();
		frame.setVisible(true);

		return frame;
	}

	public static Board createAndShowBoard() {
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
				boardHolder[0] = createAndShowGUI();
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
		Board board = createAndShowBoard();
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
	public void setPiece(final int col, final int row, final String piece) {
		String gName = piece;
		Square sq = squares[getIndex(col, row)];
		sq.label.setIcon(getPiece(gName));
		sq.graphicName = gName;
	}

	/** @return The piece category such as black-rook */
	public String getPiece(final int col, final int row) {
		Square sq = squares[getIndex(col, row)];
		return sq.graphicName;
	}

	public void showWon(final String colour, final String wonMode) {
		final JPanel boardPanel = new JPanel();
		boardPanel.add(new JButton("Won by " + colour + " with " + wonMode));
		setTitle(String.format("Won by %s with %s", colour,  wonMode));
	}
}