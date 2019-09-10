package com.weis.darklaf.decorators;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

/**
 * Wrapper interface to shorten code when only mouse clicked is used.
 *
 * @author Jannis Weis
 * @since 2019
 */
public interface MouseClickListener extends MouseListener {

    @Override
    void mouseClicked(MouseEvent e);

    @Override
    default void mousePressed(final MouseEvent e) {
    }

    @Override
    default void mouseReleased(final MouseEvent e) {
    }

    @Override
    default void mouseEntered(final MouseEvent e) {
    }

    @Override
    default void mouseExited(final MouseEvent e) {
    }
}
