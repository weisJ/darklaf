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
import java.awt.event.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;

/**
 * @author Jannis Weis
 */
public class ClosableTabComponent extends JPanel {

    private JTabbedPane pane;

    public ClosableTabComponent(final JTabbedPane pane) {
        super(new FlowLayout(FlowLayout.LEFT, 0, 0));
        if (pane == null) {
            throw new NullPointerException("TabbedPane is null.");
        }
        this.pane = pane;
        setOpaque(false);
        add(new TabLabel(this));
        add(new TabButton(this));
    }

    public void setTabbedPane(final JTabbedPane pane) {
        this.pane = pane;
    }

    protected static class TabLabel extends JLabel {

        private final ClosableTabComponent tabComponent;

        protected TabLabel(final ClosableTabComponent tabComponent) {
            this.tabComponent = tabComponent;
            setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
        }

        @Override
        public String getText() {
            if (tabComponent == null) return "";
            int i = tabComponent.pane.indexOfTabComponent(tabComponent);
            if (i != -1) {
                return tabComponent.pane.getTitleAt(i);
            }
            return "";
        }
    }

    protected static class TabButton extends JButton implements ActionListener {

        private final ClosableTabComponent tabComponent;

        protected TabButton(final ClosableTabComponent tabComponent) {
            this.tabComponent = tabComponent;
            putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_ONLY_LABEL);
            putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
            setOpaque(false);
            setRolloverEnabled(true);
            setIcon(UIManager.getIcon("TabbedPane.tabCloseIcon"));
            setRolloverIcon(UIManager.getIcon("TabbedPane.tabCloseHoverIcon"));
            addActionListener(this);
            MouseListener mouseListener = new MouseAdapter() {
                @Override
                public void mouseEntered(final MouseEvent e) {
                    ComponentUI ui = tabComponent.pane.getUI();
                    if (ui instanceof DarkTabbedPaneUI) {
                        int i = tabComponent.pane.indexOfTabComponent(tabComponent);
                        if (i != -1) {
                            ((DarkTabbedPaneUI) ui).setRolloverTab(i);
                            tabComponent.pane.repaint();
                        }
                    }
                }
            };
            addMouseListener(mouseListener);
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            int i = tabComponent.pane.indexOfTabComponent(tabComponent);
            if (i != -1) {
                tabComponent.pane.remove(i);
            }
        }
    }
}
