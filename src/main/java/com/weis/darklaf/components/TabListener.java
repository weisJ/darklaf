package com.weis.darklaf.components;

import java.util.EventListener;

/**
 * @author Jannis Weis
 */
public interface TabListener extends EventListener {

    void tabOpened(TabEvent e);

    void tabClosed(TabEvent e);
}
