package chess.ui;

import java.awt.Container;

/**
 * The API to a component allowing a user to select a move.
 */
public interface MoveEntry {

    void enable();

    void disable();

    Container getContainer();

    /**
     * @param listener The object that will receive entered move notifications
     */
    void addMoveEntryListener(MoveEntryListener listener);

    /**
     * Remove all user input from move entry interface
     */
    void clearMoveEntry();

}
