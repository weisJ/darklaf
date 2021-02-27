/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;
import java.util.Arrays;

import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.util.Alignment;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class TabFrameLayout implements LayoutManager {

    private final JTabFrame tabFrame;
    private final DarkTabFrameUI ui;
    private final int[] shift;
    private final boolean[] draggedOver;
    private int topHeight;
    private int leftHeight;
    private int rightHeight;
    private int bottomHeight;

    public TabFrameLayout(final JTabFrame tabFrame, final DarkTabFrameUI ui) {
        this.tabFrame = tabFrame;
        this.ui = ui;
        shift = new int[4];
        draggedOver = new boolean[4];
    }

    @Override
    public void addLayoutComponent(final String name, final Component comp) {}

    @Override
    public void removeLayoutComponent(final Component comp) {}

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        Dimension b = tabFrame.getContentPane().getComponent().getPreferredSize();
        return new Dimension(
                tabFrame.getLeftTabContainer().getWidth() + tabFrame.getRightTabContainer().getWidth() + b.width,
                tabFrame.getTopTabContainer().getHeight() + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        Dimension b = tabFrame.getContentPane().getComponent().getMinimumSize();
        return new Dimension(
                tabFrame.getLeftTabContainer().getWidth() + tabFrame.getRightTabContainer().getWidth() + b.width,
                tabFrame.getTopTabContainer().getHeight() + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @Override
    public void layoutContainer(final Container parent) {
        Dimension dim = parent.getSize();
        int topCount = tabFrame.getTopTabCount();
        int bottomCount = tabFrame.getBottomTabCount();
        int leftCount = tabFrame.getLeftTabCount();
        int rightCount = tabFrame.getRightTabCount();

        if (isDraggedOver(Alignment.NORTH)) topCount++;
        if (isDraggedOver(Alignment.SOUTH)) bottomCount++;
        if (isDraggedOver(Alignment.EAST)) rightCount++;
        if (isDraggedOver(Alignment.WEST)) leftCount++;

        ui.getDropComponentBottom().setSize(0, 0);
        ui.getDropComponentLeft().setSize(0, 0);
        ui.getDropComponentRight().setSize(0, 0);
        ui.getDropComponentTop().setSize(0, 0);

        topHeight = calculateMaxTabSize(Alignment.NORTH);
        bottomHeight = calculateMaxTabSize(Alignment.SOUTH);
        leftHeight = calculateMaxTabSize(Alignment.WEST);
        rightHeight = calculateMaxTabSize(Alignment.EAST);

        layoutTopTab(dim, topCount, leftCount, rightCount);
        layoutBottomTab(dim, bottomCount, leftCount, rightCount);
        layoutLeftTab(dim, leftCount);
        layoutRightTab(dim, rightCount);

        Component leftPane = ui.getLeftContainer();
        Component rightPane = ui.getRightContainer();
        Component topPane = ui.getTopContainer();
        Component bottomPane = ui.getBottomContainer();
        tabFrame.getContentPane().getComponent().setBounds(leftPane.getWidth(), topPane.getHeight(),
                dim.width - leftPane.getWidth() - rightPane.getWidth(),
                dim.height - topPane.getHeight() - bottomPane.getHeight());
    }

    protected void layoutTopTab(final Dimension dim, final int topCount, final int leftCount, final int rightCount) {
        Component topComp = tabFrame.getTopTabContainer();
        if (topCount > 0) {
            topComp.setBounds(0, 0, dim.width, topHeight);
            layoutHorizontal(dim, Alignment.NORTH, Alignment.NORTH_EAST, 0, leftCount, rightCount, topHeight);
        } else if (draggedOver[getIndex(Alignment.NORTH)]) {
            topComp.setBounds(0, 0, dim.width, topHeight);
            if (ui.getDestIndex() >= -1) {
                layoutHorizontalDrop(Alignment.NORTH, leftCount, rightCount, topHeight, 0);
            }
        } else {
            topComp.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutBottomTab(final Dimension dim, final int bottomCount, final int leftCount,
            final int rightCount) {
        Component bottomComp = tabFrame.getBottomTabContainer();
        if (bottomCount > 0) {
            bottomComp.setBounds(0, dim.height - bottomHeight, dim.width, bottomHeight);
            layoutHorizontal(dim, Alignment.SOUTH_WEST, Alignment.SOUTH, 1, leftCount, rightCount, bottomHeight);
        } else if (draggedOver[getIndex(Alignment.SOUTH)]) {
            bottomComp.setBounds(0, dim.height - bottomHeight, dim.width, bottomHeight);
            if (ui.getDestIndex() >= -1) {
                layoutHorizontalDrop(Alignment.SOUTH_WEST, leftCount, rightCount, bottomHeight, 1);
            }
        } else {
            bottomComp.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutHorizontalDrop(final Alignment left, final int leftCount, final int rightCount, final int size,
            final int yOff) {
        Alignment a = ui.getDestAlign();
        Dimension dropSize = ui.getDropSize();
        Component dropComp = ui.getDropComponent(left);
        Component tabComp = ui.getTabContainer(left);
        if (a == left) {
            int x = leftCount > 0 ? leftHeight : 0;
            dropComp.setBounds(x, yOff, dropSize.width, size);
        } else {
            int x = rightCount > 0 ? tabComp.getWidth() - rightHeight : tabComp.getWidth();
            dropComp.setBounds(x - dropSize.width, yOff, dropSize.width, size);
        }
    }

    protected void layoutHorizontal(final Dimension dim, final Alignment left, final Alignment right, final int yOff,
            final int leftCount, final int rightCount, final int tabHeight) {
        Point start = new Point(leftCount > 0 ? leftHeight : 0, yOff);
        int leftEnd = layoutTabArea(start, left, true, tabHeight - 1);
        start.x = rightCount > 0 ? dim.width - rightHeight : dim.width;
        int rightStart = layoutTabArea(start, right, false, tabHeight - 1);
        if (rightStart < leftEnd) {
            shift[getIndex(left)] = leftEnd - rightStart;
            shift(leftEnd - rightStart, right);
        } else {
            shift[getIndex(left)] = 0;
        }
    }

    protected void layoutLeftTab(final Dimension dim, final int leftCount) {
        Component leftPane = ui.getLeftContainer();
        Component topPane = tabFrame.getTopTabContainer();
        Component bottomPane = tabFrame.getBottomTabContainer();
        if (leftCount > 0 || draggedOver[getIndex(Alignment.WEST)]) {
            int size = leftCount > 0 ? leftHeight : tabFrame.getTabSize();
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            leftPane.setBounds(0, topPane.getHeight(), size, height + (height % 2));
            tabFrame.getLeftTabContainer().setPreferredSize(new Dimension(leftPane.getHeight(), leftPane.getWidth()));
            tabFrame.getLeftTabContainer().setSize(tabFrame.getLeftTabContainer().getPreferredSize());
            if (leftCount > 0) {
                Point start = new Point(leftPane.getHeight(), 0);
                int topStart = layoutTabArea(start, Alignment.NORTH_WEST, false, size - 1);
                start.x = 0;
                int bottomEnd = layoutTabArea(start, Alignment.WEST, true, size - 1);
                if (bottomEnd > topStart) {
                    shift[getIndex(Alignment.WEST)] = topStart - bottomEnd;
                    shift(topStart - bottomEnd, Alignment.WEST);
                } else {
                    shift[getIndex(Alignment.WEST)] = 0;
                }
            } else if (ui.getDestIndex() >= -1) {
                layoutVerticalDrop(Alignment.WEST, size);
            }
        } else {
            tabFrame.getLeftTabContainer().setBounds(0, 0, 0, 0);
            leftPane.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutVerticalDrop(final Alignment left, final int size) {
        Component comp = ui.getDropComponent(left);
        Alignment a = ui.getDestAlign();
        Dimension dropSize = ui.getDropSize();
        Component tabComp = tabFrame.getTabContainer(left);
        if (a == left) {
            comp.setBounds(0, 0, dropSize.width, size);
        } else {
            comp.setBounds(tabComp.getWidth() - dropSize.width, 0, dropSize.width, size);
        }
    }

    protected void layoutRightTab(final Dimension dim, final int rightCount) {
        Component rightPane = ui.getRightContainer();
        Component topPane = tabFrame.getTopTabContainer();
        Component bottomPane = tabFrame.getBottomTabContainer();
        if (rightCount > 0 || draggedOver[getIndex(Alignment.EAST)]) {
            int size = rightCount > 0 ? rightHeight : tabFrame.getTabSize();
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            rightPane.setBounds(dim.width - rightHeight, topPane.getHeight(), size, height + (height % 2));
            tabFrame.getRightTabContainer()
                    .setPreferredSize(new Dimension(rightPane.getHeight(), rightPane.getWidth()));
            tabFrame.getRightTabContainer().setSize(tabFrame.getRightTabContainer().getPreferredSize());
            if (rightCount > 0) {
                Point start = new Point(0, 0);
                int topEnd = layoutTabArea(start, Alignment.EAST, true, size - 1);
                start.x = tabFrame.getRightTabContainer().getWidth();
                int bottomStart = layoutTabArea(start, Alignment.SOUTH_EAST, false, size - 1);
                if (bottomStart < topEnd) {
                    shift[getIndex(Alignment.EAST)] = topEnd - bottomStart;
                    shift(topEnd - bottomStart, Alignment.SOUTH_EAST);
                } else {
                    shift[getIndex(Alignment.EAST)] = 0;
                }
            } else if (ui.getDestIndex() >= -1) {
                layoutVerticalDrop(Alignment.EAST, size);
            }
        } else {
            tabFrame.getRightTabContainer().setBounds(0, 0, 0, 0);
            rightPane.setBounds(0, 0, 0, 0);
        }
    }

    protected void shift(final int shift, final Alignment a) {
        for (TabFrameTab c : tabFrame.tabsForAlignment(a)) {
            Point pos = c.getComponent().getLocation();
            pos.x += shift;
            c.getComponent().setLocation(pos);
        }
        if (a == ui.getDestAlign()) {
            Component dropComp = ui.getDropComponent(a);
            Point pos = dropComp.getLocation();
            pos.x += shift;
            dropComp.setLocation(pos);
        }
    }

    protected int getIndex(final Alignment a) {
        switch (a) {
            case NORTH:
            case NORTH_EAST:
                return 0;
            case EAST:
            case SOUTH_EAST:
                return 1;
            case SOUTH:
            case SOUTH_WEST:
                return 2;
            case WEST:
            case NORTH_WEST:
                return 3;
        }
        return 0;
    }

    public void setDraggedOver(final Alignment a, final boolean b) {
        draggedOver[getIndex(a)] = b;
    }

    protected int layoutTabArea(final Point start, final Alignment a, final boolean forward, final int size) {
        int x = start.x;
        int y = start.y;
        int sourceIndex = a == ui.getSourceAlign() ? ui.getSourceIndex() : -10;
        int destIndex = a == ui.getDestAlign() ? ui.getDestIndex() : -10;

        Rectangle bounds = new Rectangle(0, 0, 0, 0);
        int index = 0;
        Component dropComp = ui.getDropComponent(a);
        if (destIndex == -1) {
            if (forward) {
                dropComp.setBounds(x, y, ui.getDropSize().width, size);
                x += ui.getDropSize().width;
            } else {
                x -= ui.getDropSize().width;
                dropComp.setBounds(x, y, ui.getDropSize().width, size);
            }
        }
        for (TabFrameTab c : tabFrame.tabsForAlignment(a)) {
            index = c.getIndex();
            bounds.width = index == sourceIndex ? 0 : getTabWidth(c.getComponent());
            bounds.height = size;
            if (forward) {
                bounds.x = x;
                bounds.y = y;
                x += bounds.width;
                if (index == destIndex) {
                    dropComp.setBounds(x, y, ui.getDropSize().width, size);
                    x += ui.getDropSize().width;
                }
            } else {
                x -= bounds.width;
                bounds.x = x;
                bounds.y = y;
                if (index == destIndex) {
                    x -= ui.getDropSize().width;
                    dropComp.setBounds(x, y, ui.getDropSize().width, size);
                }
            }
            c.getComponent().setBounds(bounds);
        }
        if (destIndex == index + 1) {
            if (forward) {
                dropComp.setBounds(x, y, ui.getDropSize().width, size);
                x += ui.getDropSize().width;
            } else {
                x -= ui.getDropSize().width;
                dropComp.setBounds(x, y, ui.getDropSize().width, size);
            }
        }
        return x;
    }

    public int getTabWidth(final Component c) {
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
        for (TabFrameTab c : tabFrame.tabsForAlignment(a)) {
            max = Math.max(max, c.getComponent().getPreferredSize().height + 1);
        }
        for (TabFrameTab c : tabFrame.tabsForAlignment(tabFrame.getPeer(a))) {
            max = Math.max(max, c.getComponent().getPreferredSize().height + 1);
        }
        return max;
    }

    public void setDraggedOver(final boolean b) {
        Arrays.fill(draggedOver, b);
    }

    public boolean isDraggedOver(final Alignment a) {
        return draggedOver[getIndex(a)];
    }
}
