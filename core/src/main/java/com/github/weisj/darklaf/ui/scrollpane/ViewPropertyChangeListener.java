/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;

public class ViewPropertyChangeListener implements PropertyChangeListener {

    private final JScrollPane scrollPane;
    private final PropertyChangeListener listener;
    private final ContainerListener containerListener = new ContainerAdapter() {
        @Override
        public void componentAdded(final ContainerEvent e) {
            if (currentView != null) {
                currentView.removePropertyChangeListener(listener);
            }
            currentView = e.getChild();
            if (currentView != null) {
                currentView.addPropertyChangeListener(listener);
            }
        }
    };
    private Component currentView;

    public ViewPropertyChangeListener(final JScrollPane scrollPane, final PropertyChangeListener listener) {
        this.scrollPane = scrollPane;
        this.listener = listener;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        String key = e.getPropertyName();
        Object source = e.getSource();
        if (source == scrollPane) {
            if ("viewport".equals(key)) {
                Object old = e.getOldValue();
                Object newVal = e.getNewValue();
                if (old instanceof JViewport) {
                    ((JViewport) old).addContainerListener(containerListener);
                }
                if (newVal instanceof JViewport) {
                    ((JViewport) newVal).removeContainerListener(containerListener);
                }
            }
        }
    }

    public void install() {
        scrollPane.addPropertyChangeListener(this);
        JViewport viewport = scrollPane.getViewport();
        if (viewport != null) {
            viewport.addContainerListener(containerListener);
            Component view = viewport.getView();
            if (view != null) {
                view.addPropertyChangeListener(listener);
            }
        }
    }

    public void uninstall() {
        scrollPane.removePropertyChangeListener(this);
        JViewport viewport = scrollPane.getViewport();
        if (viewport != null) {
            viewport.removeContainerListener(containerListener);
            Component view = viewport.getView();
            if (view != null) {
                view.removePropertyChangeListener(listener);
            }
        }
    }
}
