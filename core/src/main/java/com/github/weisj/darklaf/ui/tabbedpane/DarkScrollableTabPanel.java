/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;

class DarkScrollableTabPanel extends ScrollableTabPanel {

    protected final Rectangle iconRect = new Rectangle();
    protected final Rectangle textRect = new Rectangle();
    private final DarkTabbedPaneUI tabbedPaneUI;

    public DarkScrollableTabPanel(final DarkTabbedPaneUI tabbedPaneUI) {
        super(tabbedPaneUI);
        this.tabbedPaneUI = tabbedPaneUI;
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (tabbedPaneUI.drawDropRect) {
            tabbedPaneUI.paintDrop(g);
        }
    }

    @Override
    public void doLayout() {
        if (getComponentCount() > 0) {
            for (int i = 0; i < getComponentCount(); i++) {
                Component child = getComponent(i);
                if (child == tabbedPaneUI.scrollableTabSupport.newTabButton) {
                    boolean leftToRight = tabbedPaneUI.tabPane.getComponentOrientation().isLeftToRight();
                    int tabCount = tabbedPaneUI.tabPane.getTabCount();
                    Dimension b = child.getPreferredSize();
                    if (tabCount > 0) {
                        if (tabbedPaneUI.isHorizontalTabPlacement()) {
                            int off = tabbedPaneUI.dropTargetIndex == tabCount ? tabbedPaneUI.dropRect.width : 0;
                            if (leftToRight) {
                                int x = tabbedPaneUI.rects[tabCount - 1].x + tabbedPaneUI.rects[tabCount - 1].width
                                        + off;
                                child.setBounds(x, 0, b.width, tabbedPaneUI.maxTabHeight);
                            } else {
                                int x = tabbedPaneUI.rects[tabCount - 1].x - off;
                                child.setBounds(x - b.width, 0, b.width, tabbedPaneUI.maxTabHeight);
                            }
                        } else {
                            int off = tabbedPaneUI.dropTargetIndex == tabCount ? tabbedPaneUI.dropRect.height : 0;
                            int y = tabbedPaneUI.rects[tabCount - 1].y + tabbedPaneUI.rects[tabCount - 1].height + off;
                            child.setBounds(0, y, tabbedPaneUI.maxTabWidth, b.height);
                        }
                    }
                } else {
                    child.setBounds(0, 0, getWidth(), getHeight());
                }
            }
        }
    }

    @Override
    public void paint(final Graphics g) {
        super.paint(g);
        if (tabbedPaneUI.dragging && tabbedPaneUI.tabPane.getTabCount() > 0) {
            tabbedPaneUI.paintTab(g, tabbedPaneUI.tabPane.getTabPlacement(), tabbedPaneUI.dragRect,
                    tabbedPaneUI.tabPane.getSelectedIndex(), iconRect,
                    textRect);
            Component comp = tabbedPaneUI.tabPane.getTabComponentAt(tabbedPaneUI.dropSourceIndex);
            if (comp != null) {
                g.translate(comp.getX(), comp.getY());
                comp.print(g);
            }
        }
    }
}
