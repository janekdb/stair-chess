package chess.ui;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * Provides a text based move entry UI element. Adapts input events to the
 * <code>MoveEntryListener</code> interface.
 */
public class InputFieldMoveEntry implements MoveEntry {

	private final JPanel contentPane = new JPanel(new BorderLayout());

	private final JTextField inputField = new JTextField(4);

	public InputFieldMoveEntry() {
		contentPane.add(inputField, BorderLayout.NORTH);
	}

	@Override
	public void enable() {
		throw new RuntimeException("TODO");
	}

	@Override
	public void disable() {
		throw new RuntimeException("TODO");
	}

	@Override
	public Container getContainer() {
		return contentPane;
	}

	@Override
	public void addMoveEntryListener(final MoveEntryListener listener) {
		ActionListener a = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				listener.onMoveEntry(e.getActionCommand());
			}
		};
		inputField.addActionListener(a);
	}

	@Override
	public void clearMoveEntry() {
		inputField.setText("");
	}

}
