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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.awt.event.*;
import java.util.function.BiFunction;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.github.weisj.darklaf.util.ResourceUtil;

/**
 * @author Jannis Weis
 */
public class ClosableTabComponent extends JPanel {

    private JTabbedPane pane;
    private final Component component;
    private boolean closable;
    private final CloseButton closeButton;

    public ClosableTabComponent(final JTabbedPane pane) {
        this(pane, null);
    }

    public ClosableTabComponent(final JTabbedPane pane, final Component component) {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        this.component = component != null ? component : new TabLabel(this);
        if (pane == null) {
            throw new NullPointerException("TabbedPane is null.");
        }
        this.pane = pane;
        closable = true;
        closeButton = new TabCloseButton(this);
        setOpaque(false);
        add(getTabComponent());
        add(getCloseButton());
    }

    public Component getTabComponent() {
        return component;
    }

    public CloseButton getCloseButton() {
        return closeButton;
    }

    public boolean isClosable() {
        return closable;
    }

    public void setClosable(final boolean closable) {
        if (this.closable != closable) {
            this.closable = closable;
            if (!closable) {
                remove(closeButton);
            } else {
                add(closeButton);
            }
            doLayout();
        }
    }

    public void setTabbedPane(final JTabbedPane pane) {
        this.pane = pane;
    }

    public int getIndexInTabbedPane() {
        return pane.indexOfTabComponent(this);
    }

    @Override
    public void setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
        getTabComponent().setEnabled(enabled);
        getCloseButton().setEnabled(enabled);
    }

    protected static class TabLabel extends JLabel {

        private final ClosableTabComponent tabComponent;

        protected TabLabel(final ClosableTabComponent tabComponent) {
            this.tabComponent = tabComponent;
            setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
        }

        protected <T> T getTabProperty(final BiFunction<JTabbedPane, Integer, T> mapper, final T fallback) {
            if (tabComponent != null) {
                int i = tabComponent.getIndexInTabbedPane();
                if (i != -1) {
                    return mapper.apply(tabComponent.pane, i);
                }
            }
            return fallback;
        }

        @Override
        public Icon getIcon() {
            return getTabProperty(JTabbedPane::getIconAt, null);
        }

        @Override
        public Icon getDisabledIcon() {
            return getTabProperty(JTabbedPane::getDisabledIconAt, null);
        }

        @Override
        public int getDisplayedMnemonic() {
            return getTabProperty(JTabbedPane::getDisplayedMnemonicIndexAt, -1);
        }

        @Override
        public String getText() {
            return getTabProperty(JTabbedPane::getTitleAt, "");
        }
    }

    protected static class TabCloseButton extends CloseButton implements ActionListener {

        private final ClosableTabComponent tabComponent;

        protected TabCloseButton(final ClosableTabComponent tabComponent) {
            this.tabComponent = tabComponent;
            addActionListener(this);
            MouseListener mouseListener = new MouseAdapter() {
                @Override
                public void mouseEntered(final MouseEvent e) {
                    ComponentUI ui = tabComponent.pane.getUI();
                    if (ui instanceof DarkTabbedPaneUI) {
                        int i = tabComponent.pane.indexOfTabComponent(tabComponent);
                        if (i != -1) {
                            ((DarkTabbedPaneUI) ui).setRolloverTab(i);
                            tabComponent.pane.repaint(tabComponent.pane.getBoundsAt(i));
                        }
                    }
                }
            };
            addMouseListener(mouseListener);
            setToolTipText(
                ResourceUtil.getResourceBundle("actions", tabComponent.pane).getString("Actions.closableTabbedPane.close")
            );
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if ((e.getModifiers() & ActionEvent.ALT_MASK) != 0) {
                // Close all other tabs.
                int tabCount = tabComponent.pane.getTabCount();
                for (int i = tabCount - 1; i >= 0; i--) {
                    Component tabComp = tabComponent.pane.getTabComponentAt(i);
                    if (tabComp == tabComponent) continue;
                    if (tabComp instanceof ClosableTabComponent && ((ClosableTabComponent) tabComp).isClosable()) {
                        tabComponent.pane.removeTabAt(i);
                    }
                }
            } else {
                int i = tabComponent.pane.indexOfTabComponent(tabComponent);
                if (i != -1) {
                    tabComponent.pane.remove(i);
                }
            }
        }
    }
}
