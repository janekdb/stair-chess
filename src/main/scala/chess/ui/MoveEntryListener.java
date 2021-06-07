package chess.ui;

/**
 * Classes implementing this interface can receive notifications when a move has
 * been entered
 */
public interface MoveEntryListener {

    void onMoveEntry(String text);
}
