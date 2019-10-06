package com.weis.darklaf.decorators;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class PlainAction extends AbstractAction {

    private final Runnable action;

    public PlainAction(final String name, final Runnable action) {
        super(name);
        this.action = action;
    }

    public PlainAction(final String name, final Icon icon, final Runnable action) {
        super(name, icon);
        this.action = action;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        action.run();
    }
}
