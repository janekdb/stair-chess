package chess.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.util.concurrent.CountDownLatch;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.UIManager;
 
public class SwingBoard extends JFrame implements Board {
    GridLayout boardLayout = new GridLayout(8,8);
     
    public SwingBoard(String name) {
        super(name);
        setResizable(false);
    }
     
    private static final int BOARD_SIZE = 8;
     
    private static final int SQUARE_SIZE = 120;
    
    private static final Color WHITE_BACKGROUND = Color.WHITE;
    private static final Color BLACK_BACKGROUND = Color.LIGHT_GRAY;
    private static final Color [] BACKGROUNDS = new Color[] {WHITE_BACKGROUND, BLACK_BACKGROUND};
    
    private Color getBackgroundColor(final int col, final int row){
    	int s = (row-1) * (BOARD_SIZE +1)+ (col-1) + 1;
    	return BACKGROUNDS[s%2];
    }
    
    /** One-based */
    private int getIndex(final int col, final int row){
    	return (row-1) * BOARD_SIZE + (col-1);
    }
    
    // TODO: Better name than buttons
    private JButton [] squares;
    
    private String columnLabels = "abcdefgh";
    
    public void addComponentsToPane(final Container pane) {
        final JPanel boardPanel = new JPanel();
        boardPanel.setLayout(boardLayout);
        JPanel controls = new JPanel();
        controls.setLayout(new GridLayout(2,3));
         
        boardPanel.setPreferredSize(new Dimension(SQUARE_SIZE
				* BOARD_SIZE, SQUARE_SIZE * BOARD_SIZE));
         
        squares = new JButton[BOARD_SIZE * BOARD_SIZE];
        
        	for(int row = BOARD_SIZE; row >= 1 ; row--){
        		for(int col = 1; col <= BOARD_SIZE; col ++){
        		JButton b = new JButton(columnLabels.substring(col-1, col) +row);
        		Color bg = getBackgroundColor(col, row);
        		b.setBackground(bg);
        		boardPanel.add(b);
        		squares[getIndex(col, row)] = b;
        	}
        }

        pane.add(boardPanel, BorderLayout.CENTER);
    }
     
    /**
     * Create the GUI and show it.  For thread safety,
     * this method is invoked from the
     * event dispatch thread.
     */
    private static SwingBoard createAndShowGUI() {
        //Create and set up the window.
        SwingBoard frame = new SwingBoard("GridLayoutDemo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //Set up the content pane.
        frame.addComponentsToPane(frame.getContentPane());
        //Display the window.
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
    
	// TODO: Use Unicode for pieces
	public static void main(String[] args) {
		Board board = createAndShowBoard();
	}
	
	// TODO: Move these operations into an interface.
	public void setLabel(final int col, final int row, final String label){
		JButton b = squares[getIndex(col, row)];
   		b.setText(label);
	}
	
	public String getLabel(final int col, final int row) {
		JButton b = squares[getIndex(col, row)];
		return b.getText();
	}
}