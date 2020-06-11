/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.*;

/**
 * @author Jannis Weis
 */
public class ClosableTabbedPane extends JTabbedPane {

    @Override
    public void insertTab(final String title, final Icon icon, final Component component,
                          final String tip, final int index) {
        if (notifyVetoableChangeListeners(new TabPropertyChangeEvent(this, "tabOpened",
                                                                     null, component, index))) {
            return;
        }
        super.insertTab(title, icon, component, tip, index);
        setTabComponentAt(indexOfComponent(component), new ClosableTabComponent(this));
        notifyTabListeners(new TabEvent(this, TabEvent.Type.TAB_OPENED, "tabOpened", index, component));
    }

    @Override
    public void removeTabAt(final int index) {
        checkIndex(index);
        Component c = getComponentAt(index);
        if (notifyVetoableChangeListeners(new TabPropertyChangeEvent(this, "tabClosed",
                                                                     getComponentAt(index), null, index))) {
            return;
        }
        notifyTabListeners(new TabEvent(this, TabEvent.Type.TAB_CLOSING, "tabClosing", index, c));
        super.removeTabAt(index);
        notifyTabListeners(new TabEvent(this, TabEvent.Type.TAB_CLOSED, "tabClosed", index, c));
    }

    @Override
    public void setTabComponentAt(final int index, final Component component) {
        if (component instanceof ClosableTabComponent) {
            ((ClosableTabComponent) component).setTabbedPane(this);
            super.setTabComponentAt(index, component);
        } else {
            super.setTabComponentAt(index, new ClosableTabComponent(this, component));
        }
    }

    private void checkIndex(final int index) {
        int tabCount = getTabCount();
        if (index < 0 || index >= tabCount) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Tab count: " + tabCount);
        }
    }

    private boolean notifyVetoableChangeListeners(final TabPropertyChangeEvent e) {
        try {
            VetoableChangeListener[] listeners = getVetoableChangeListeners();
            for (VetoableChangeListener l : listeners) {
                l.vetoableChange(e);
            }
        } catch (PropertyVetoException ex) {
            return true;
        }
        return false;
    }

    private void notifyTabListeners(final TabEvent event) {
        TabListener[] listeners = listenerList.getListeners(TabListener.class);
        switch (event.getID()) {
            case TabEvent.TAB_CLOSED :
                for (TabListener l : listeners) {
                    l.tabClosed(event);
                }
                break;
            case TabEvent.TAB_OPENED :
                for (TabListener l : listeners) {
                    l.tabOpened(event);
                }
                break;
        }
    }

    public void addTabListener(final TabListener listener) {
        listenerList.add(TabListener.class, listener);
    }

    public void removeTabListener(final TabListener listener) {
        listenerList.remove(TabListener.class, listener);
    }
}
