/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.tabframe;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.tabframe.TabFrame;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class TabFrameLayout implements LayoutManager {

    private final TabFrame tabFrame;
    private DarkTabFrameUI ui;
    private int maxTabHeight;
    private int maxTabWidth;

    @Contract(pure = true)
    public TabFrameLayout(@NotNull final TabFrame tabFrame, final DarkTabFrameUI ui) {
        this.tabFrame = tabFrame;
        this.ui = ui;
    }

    @Override
    public void addLayoutComponent(final String name, final Component comp) {
    }

    @Override
    public void removeLayoutComponent(final Component comp) {
    }

    @NotNull
    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        var b = tabFrame.getContentPane().getComponent().getPreferredSize();
        return new Dimension(tabFrame.getLeftTabContainer().getWidth()
                                     + tabFrame.getRightTabContainer().getWidth() + b.width,
                             tabFrame.getTopTabContainer().getHeight()
                                     + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @NotNull
    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        var b = tabFrame.getContentPane().getComponent().getMinimumSize();
        return new Dimension(tabFrame.getLeftTabContainer().getWidth()
                                     + tabFrame.getRightTabContainer().getWidth() + b.width,
                             tabFrame.getTopTabContainer().getHeight()
                                     + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @Override
    public void layoutContainer(@NotNull final Container parent) {
        var dim = parent.getSize();
        int topSize = tabFrame.getTopTabCount();
        int bottomSize = tabFrame.getBottomTabCount();
        int leftSize = tabFrame.getLeftTabCount();
        int rightSize = tabFrame.getRightTabCount();
        layoutTopTab(dim, topSize, leftSize, rightSize);
        layoutBottomTab(dim, bottomSize, leftSize, rightSize);
        layoutLeftTab(dim, leftSize);
        layoutRightTab(dim, rightSize);

        ui.updateBorders(topSize, bottomSize, leftSize, rightSize);

        var leftPane = ui.getLeftContainer();
        var rightPane = ui.getRightContainer();
        var topPane = ui.getTopContainer();
        var bottomPane = ui.getBottomContainer();
        tabFrame.getContentPane().getComponent().setBounds(leftPane.getWidth(), topPane.getHeight(),
                                                           dim.width - leftPane.getWidth() - rightPane.getWidth(),
                                                           dim.height - topPane.getHeight() - bottomPane.getHeight());
    }

    protected void layoutTopTab(final Dimension dim, final int topSize, final int leftSize, final int rightSize) {
        if (topSize > 0) {
            tabFrame.getTopTabContainer().setBounds(0, 0, dim.width, tabFrame.getTabSize());
            layoutHorizontal(dim, Alignment.NORTH, Alignment.NORTH_EAST, 0, leftSize, rightSize);
        } else {
            tabFrame.getTopTabContainer().setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutBottomTab(final Dimension dim, final int bottomSize, final int leftSize, final int rightSize) {
        if (bottomSize > 0) {
            int size = tabFrame.getTabSize();
            tabFrame.getBottomTabContainer().setBounds(0, dim.height - size, dim.width, size);
            layoutHorizontal(dim, Alignment.SOUTH_WEST, Alignment.SOUTH, 1, leftSize, rightSize);
        } else {
            tabFrame.getBottomTabContainer().setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutHorizontal(final Dimension dim, final Alignment left, final Alignment right,
                                    final int yOff, final int leftSize, final int rightSize) {
        int tabHeight = calculateMaxTabSize(left);
        var start = new Point(leftSize > 0 ? tabHeight : 0, yOff);
        int leftEnd = layoutTabArea(start, left, true, tabHeight - 1);
        start.x = rightSize > 0 ? dim.width - tabHeight : dim.width;
        int rightStart = layoutTabArea(start, right, false, tabHeight - 1);
        if (rightStart < leftEnd) {
            shift(leftEnd - rightStart, right);
        }
    }

    protected void layoutLeftTab(final Dimension dim, final int leftSize) {
        var leftPane = ui.getLeftContainer();
        var topPane = tabFrame.getTopTabContainer();
        var bottomPane = tabFrame.getBottomTabContainer();
        int tabWidth = calculateMaxTabSize(Alignment.WEST);
        if (leftSize > 0) {
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            leftPane.setBounds(0, topPane.getHeight(), tabWidth, height + (height % 2));
            tabFrame.getLeftTabContainer().setPreferredSize(new Dimension(leftPane.getHeight(),
                                                                          leftPane.getWidth()));
            tabFrame.getLeftTabContainer().setSize(tabFrame.getLeftTabContainer().getPreferredSize());
            var start = new Point(leftPane.getHeight(), 0);
            int topStart = layoutTabArea(start, Alignment.NORTH_WEST, false, tabWidth - 1);
            start.x = 0;
            int bottomEnd = layoutTabArea(start, Alignment.WEST, true, tabWidth - 1);
            if (bottomEnd > topStart) {
                shift(topStart - bottomEnd, Alignment.WEST);
            }
        } else {
            tabFrame.getLeftTabContainer().setBounds(0, 0, 0, 0);
            leftPane.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutRightTab(final Dimension dim, final int rightSize) {
        var rightPane = ui.getRightContainer();
        var topPane = tabFrame.getTopTabContainer();
        var bottomPane = tabFrame.getBottomTabContainer();
        int tabWidth = calculateMaxTabSize(Alignment.EAST);
        if (rightSize > 0) {
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            rightPane.setBounds(dim.width - tabWidth, topPane.getHeight(), tabWidth, height + (height % 2));
            tabFrame.getRightTabContainer().setPreferredSize(new Dimension(rightPane.getHeight(), rightPane.getWidth()));
            tabFrame.getRightTabContainer().setSize(tabFrame.getRightTabContainer().getPreferredSize());
            var start = new Point(0, 0);
            int topEnd = layoutTabArea(start, Alignment.EAST, true, tabWidth - 1);
            start.x = tabFrame.getRightTabContainer().getWidth();
            var bottomStart = layoutTabArea(start, Alignment.SOUTH_EAST, false, tabWidth - 1);
            if (bottomStart < topEnd) {
                shift(topEnd - bottomStart, Alignment.SOUTH_EAST);
            }
        } else {
            tabFrame.getRightTabContainer().setBounds(0, 0, 0, 0);
            topPane.setBounds(0, 0, 0, 0);
        }
    }

    protected void shift(final int shift, final Alignment a) {
        for (var c : tabFrame.tabsForAlignment(a)) {
            var pos = c.getComponent().getLocation();
            pos.x += shift;
            c.getComponent().setLocation(pos);
        }
    }

    protected int layoutTabArea(@NotNull final Point start, @NotNull final Alignment a,
                                final boolean forward, final int size) {
        int x = start.x;
        int y = start.y;
        var bounds = new Rectangle(0, 0, 0, 0);
        for (var c : tabFrame.tabsForAlignment(a)) {
            bounds.width = getTabWidth(c.getComponent());
            bounds.height = size;
            if (forward) {
                bounds.x = x;
                bounds.y = y;
                x += bounds.width;
            } else {
                x -= bounds.width;
                bounds.x = x;
                bounds.y = y;
            }
            c.getComponent().setBounds(bounds);
        }
        return x;
    }

    protected int getTabWidth(@NotNull final Component c) {
        int maxWidth = tabFrame.getMaxTabWidth();
        int width = c.getPreferredSize().width;
        if (maxWidth < 0) {
            return width;
        } else {
            return Math.min(maxWidth, width);
        }
    }

    protected int calculateMaxTabSize(final Alignment a) {
        int max = tabFrame.getTabSize();
        for (var c : tabFrame.tabsForAlignment(a)) {
            max = Math.max(max, c.getComponent().getMaximumSize().height + 1);
        }
        for (var c : tabFrame.tabsForAlignment(tabFrame.getPeer(a))) {
            max = Math.max(max, c.getComponent().getMaximumSize().height + 1);
        }
        return max;
    }
}
