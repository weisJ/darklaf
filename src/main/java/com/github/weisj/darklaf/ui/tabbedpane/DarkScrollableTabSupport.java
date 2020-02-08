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
 */
package com.github.weisj.darklaf.ui.tabbedpane;

import com.github.weisj.darklaf.components.ScrollPopupMenu;
import com.github.weisj.darklaf.decorators.PopupMenuAdapter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

public class DarkScrollableTabSupport extends ScrollableTabSupport implements MouseWheelListener, ActionListener {

    protected static final int SCROLL_REWIND_DELAY = 1200;
    protected final ScrollPopupMenu scrollPopupMenu;
    protected final JButton moreTabsButton;
    protected final JComponent newTabButton;
    protected final Timer timer;
    protected long lastClickEvent;
    private DarkTabbedPaneUI ui;

    public DarkScrollableTabSupport(final DarkTabbedPaneUI ui, final int tabPlacement) {
        super(ui);
        this.ui = ui;
        viewport = new DarkScrollableTabViewport(ui);
        tabPanel = new DarkScrollableTabPanel(ui);

        viewport.setView(tabPanel);
        viewport.addMouseWheelListener(this);

        moreTabsButton = ui.createMoreTabsButton();
        moreTabsButton.setVisible(false);
        moreTabsButton.addActionListener(this);

        newTabButton = ui.createNewTabButton();
        newTabButton.setVisible(Boolean.TRUE.equals(ui.tabPane.getClientProperty("JTabbedPane.showNewTabButton")));

        scrollPopupMenu = new ScrollPopupMenu(UIManager.getInt("TabbedPane.maxPopupHeight"));
        PopupMenuListener popupMenuListener = new PopupMenuAdapter() {
            @Override
            public void popupMenuWillBecomeInvisible(@NotNull final PopupMenuEvent e) {
                lastClickEvent = System.currentTimeMillis();
            }
        };
        scrollPopupMenu.addPopupMenuListener(popupMenuListener);

        ui.tabPane.add(moreTabsButton);
        timer = new Timer(SCROLL_REWIND_DELAY, e -> endScroll());
        timer.setRepeats(false);
    }

    protected void endScroll() {
        ui.currentShiftX += ui.scrollShiftX;
        ui.currentShiftY += ui.scrollShiftY;
        ui.scrollShiftX = 0;
        ui.scrollShiftY = 0;
        ui.scrollLayout.calculateTabRects(ui.tabPane.getTabPlacement(), ui.tabPane.getTabCount());
        updateRollover();
        viewport.repaint();
    }

    protected void updateRollover() {
        Point pos = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(pos, ui.tabPane);
        ui.setRolloverTab(pos.x, pos.y);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (scrollPopupMenu.isVisible()) {
            scrollPopupMenu.setVisible(false);
        } else {
            if (!ui.tabPane.isEnabled()) return;
            if (lastClickEvent == 0 || (System.currentTimeMillis() - lastClickEvent) > 250) {
                Dimension pref = scrollPopupMenu.getPreferredSize();
                boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();
                switch (ui.tabPane.getTabPlacement()) {
                    case SwingConstants.LEFT:
                        scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth(),
                                             moreTabsButton.getHeight() - pref.height);
                        break;
                    case SwingConstants.RIGHT:
                        scrollPopupMenu.show(moreTabsButton, -pref.width,
                                             moreTabsButton.getHeight() - pref.height);
                        break;
                    case SwingConstants.TOP:
                        if (leftToRight) {
                            scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth() - pref.width,
                                                 moreTabsButton.getHeight());
                        } else {
                            scrollPopupMenu.show(moreTabsButton, 0, moreTabsButton.getHeight());
                        }
                        break;
                    case SwingConstants.BOTTOM:
                        if (leftToRight) {
                            scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth() - pref.width,
                                                 -pref.height);
                        } else {
                            scrollPopupMenu.show(moreTabsButton, 0, -pref.height);
                        }
                        break;
                }
            }
        }
    }

    public void hideMoreTabsButton() {
        moreTabsButton.setVisible(false);
    }

    @Override
    public void mouseWheelMoved(final MouseWheelEvent e) {
        if (!ui.tabPane.isEnabled() || ui.tabPane.getTabCount() == 0) return;
        int tabPosition = ui.tabPane.getTabPlacement();
        int scrollAmount = -1 * e.getUnitsToScroll() * e.getScrollAmount();
        int scrolled;
        if (tabPosition == SwingConstants.LEFT || tabPosition == SwingConstants.RIGHT) {
            if (e.isShiftDown() || !moreTabsButton.isVisible()) return;
            timer.stop();
            scrolled = scroll(scrollAmount, false);
        } else {
            if (!e.isShiftDown() || !moreTabsButton.isVisible()) return;
            timer.stop();
            scrolled = scroll(scrollAmount, true);
        }
        if (scrolled != 0) {
            showMoreTabsButton();
            updateRollover();
            viewport.repaint();
        }
        timer.start();
    }

    protected int scroll(final int amount, final boolean horizontal) {
        Dimension size = ui.tabPane.getSize();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(ui.tabPane.getTabPlacement());
        int tabCount = ui.tabPane.getTabCount();
        int shift;
        if (horizontal) {
            int rightMargin = size.width - (insets.left + insets.right
                    + tabAreaInsets.right + tabAreaInsets.left);
            if (moreTabsButton.isVisible()) {
                rightMargin -= moreTabsButton.getWidth();
            }
            int low = ui.rects[0].x;
            int high = ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width;
            shift = Math.abs(amount);
            if (amount > 0) {
                shift = Math.min(Math.max(-1 * low, 0), shift);
            } else {
                shift = Math.min(Math.max(high - rightMargin, 0), shift);
                shift *= -1;
            }
            ui.scrollLayout.commitShiftX(shift, tabCount);
            ui.scrollShiftX += shift;
        } else {
            int bottomMargin = size.height - (insets.bottom + tabAreaInsets.bottom
                    + insets.top + tabAreaInsets.top);
            if (moreTabsButton.isVisible()) {
                bottomMargin -= moreTabsButton.getHeight();
            }
            int low = ui.rects[0].y;
            int high = ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height;
            shift = Math.abs(amount);
            if (amount > 0) {
                shift = Math.min(Math.max(-1 * low, 0), shift);
            } else {
                shift = Math.min(Math.max(high - bottomMargin, 0), shift);
                shift *= -1;
            }
            ui.scrollLayout.commitShiftY(shift, tabCount);
            ui.scrollShiftY += shift;
        }
        return shift;
    }

    public void showMoreTabsButton() {
        moreTabsButton.setVisible(true);
        scrollPopupMenu.removeAll();
        if (ui.maxVisible < 0 || ui.minVisible >= ui.tabPane.getTabCount()) {
            ui.scrollLayout.updateVisibleRange(ui.tabPane.getTabPlacement());
        }
        if (ui.minVisible != ui.tabPane.getTabCount() && ui.maxVisible >= 0) {
            for (int i = 0; i < ui.minVisible; i++) {
                scrollPopupMenu.add(createMenuItem(i));
            }
        }
        for (int i = ui.maxVisible + 1; i < ui.tabPane.getTabCount(); i++) {
            scrollPopupMenu.add(createMenuItem(i));
        }
        moreTabsButton.repaint();
    }

    @NotNull
    @Contract("_ -> new")
    protected JMenuItem createMenuItem(final int i) {
        Icon icon = ui.tabPane.getIconAt(i);
        if (icon != null && !ui.tabPane.getComponentAt(i).isEnabled()) {
            icon = ui.tabPane.getDisabledIconAt(i);
        }
        Component comp = ui.tabPane.getComponentAt(i);
        return new JMenuItem(new AbstractAction(ui.tabPane.getTitleAt(i), icon) {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (i >= 0 && i <= ui.tabPane.getTabCount()) {
                    //Use component instead of index as index may have changed in between creation
                    //and invocation of action.
                    ui.tabPane.setSelectedComponent(comp);
                    ui.tabPane.doLayout();
                    comp.requestFocus();
                }
            }
        });
    }
}
