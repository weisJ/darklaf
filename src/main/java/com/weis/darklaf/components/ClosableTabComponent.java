package com.weis.darklaf.components;

import com.weis.darklaf.ui.tabbedpane.DarkTabbedPaneUI;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class ClosableTabComponent extends JPanel {

    private final JTabbedPane pane;

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

    protected static class TabLabel extends JLabel {

        private ClosableTabComponent tabComponent;

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

        private final MouseListener mouseListener = new MouseAdapter() {
            @Override
            public void mouseEntered(final MouseEvent e) {
                var ui = tabComponent.pane.getUI();
                if (ui instanceof DarkTabbedPaneUI) {
                    int i = tabComponent.pane.indexOfTabComponent(tabComponent);
                    if (i != -1) {
                        ((DarkTabbedPaneUI) ui).setRolloverTab(i);
                        tabComponent.pane.repaint();
                    }
                }
            }
        };
        private final ClosableTabComponent tabComponent;

        protected TabButton(final ClosableTabComponent tabComponent) {
            this.tabComponent = tabComponent;
            putClientProperty("JButton.variant", "onlyLabel");
            setOpaque(false);
            setRolloverEnabled(true);
            setIcon(UIManager.getIcon("TabbedPane.tabCloseIcon"));
            setRolloverIcon(UIManager.getIcon("TabbedPane.tabCloseHoverIcon"));
            addActionListener(this);
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
