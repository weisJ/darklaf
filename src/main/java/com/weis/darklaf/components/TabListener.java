package com.weis.darklaf.components;

import java.util.EventListener;

public interface TabListener extends EventListener {

    void tabOpened(TabEvent e);

    void tabClosed(TabEvent e);
}
