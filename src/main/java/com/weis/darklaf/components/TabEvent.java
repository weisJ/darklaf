package com.weis.darklaf.components;

import java.awt.event.ActionEvent;

/**
 * @author Jannis Weis
 */
public class TabEvent extends ActionEvent {

    public static final int TAB_OPENED = 0;
    public static final int TAB_CLOSED = 1;
    private final int tabIndex;

    public TabEvent(final Object source, final int id, final String command, final int tabIndex) {
        super(source, id, command);
        this.tabIndex = tabIndex;
    }

    public int getTabIndex() {
        return tabIndex;
    }
}
