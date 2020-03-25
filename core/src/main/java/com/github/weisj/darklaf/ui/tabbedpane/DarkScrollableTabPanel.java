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

import java.awt.*;

class DarkScrollableTabPanel extends ScrollableTabPanel {

    protected final Rectangle iconRect = new Rectangle();
    protected final Rectangle textRect = new Rectangle();
    private final DarkTabbedPaneUI ui;

    public DarkScrollableTabPanel(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (ui.drawDropRect) {
            ui.paintDrop(g);
        }
    }

    public void doLayout() {
        if (getComponentCount() > 0) {
            for (int i = 0; i < getComponentCount(); i++) {
                Component child = getComponent(i);
                if (child == ui.scrollableTabSupport.newTabButton) {
                    boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();
                    int tabCount = ui.tabPane.getTabCount();
                    Dimension b = child.getPreferredSize();
                    if (tabCount > 0) {
                        if (ui.isHorizontalTabPlacement()) {
                            int off = ui.dropTargetIndex == tabCount ? ui.dropRect.width : 0;
                            if (leftToRight) {
                                int x = ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width + off;
                                child.setBounds(x, 0, b.width, ui.maxTabHeight);
                            } else {
                                int x = ui.rects[tabCount - 1].x - off;
                                child.setBounds(x - b.width, 0, b.width, ui.maxTabHeight);
                            }
                        } else {
                            int off = ui.dropTargetIndex == tabCount ? ui.dropRect.height : 0;
                            int y = ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height + off;
                            child.setBounds(0, y, ui.maxTabWidth, b.height);
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
        if (ui.dragging && ui.tabPane.getTabCount() > 0) {
            ui.paintTab(g, ui.tabPane.getTabPlacement(), ui.dragRect, ui.tabPane.getSelectedIndex(), iconRect,
                        textRect);
            Component comp = ui.tabPane.getTabComponentAt(ui.dropSourceIndex);
            if (comp != null) {
                g.translate(comp.getX(), comp.getY());
                comp.print(g);
            }
        }
    }
}
