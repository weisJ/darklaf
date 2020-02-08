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
package com.github.weisj.darklaf.ui.tabframe;

import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.Arrays;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class TabFrameLayout implements LayoutManager {

    private final JTabFrame tabFrame;
    private DarkTabFrameUI ui;
    private int[] shift;
    private boolean[] draggedOver;
    private int topHeight;
    private int leftHeight;
    private int rightHeight;
    private int bottomHeight;

    @Contract(pure = true)
    public TabFrameLayout(@NotNull final JTabFrame tabFrame, final DarkTabFrameUI ui) {
        this.tabFrame = tabFrame;
        this.ui = ui;
        shift = new int[4];
        draggedOver = new boolean[4];
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
        Dimension b = tabFrame.getContentPane().getComponent().getPreferredSize();
        return new Dimension(tabFrame.getLeftTabContainer().getWidth()
                                     + tabFrame.getRightTabContainer().getWidth() + b.width,
                             tabFrame.getTopTabContainer().getHeight()
                                     + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @NotNull
    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        Dimension b = tabFrame.getContentPane().getComponent().getMinimumSize();
        return new Dimension(tabFrame.getLeftTabContainer().getWidth()
                                     + tabFrame.getRightTabContainer().getWidth() + b.width,
                             tabFrame.getTopTabContainer().getHeight()
                                     + tabFrame.getBottomTabContainer().getHeight() + b.height);
    }

    @Override
    public void layoutContainer(@NotNull final Container parent) {
        Dimension dim = parent.getSize();
        int topSize = tabFrame.getTopTabCount();
        int bottomSize = tabFrame.getBottomTabCount();
        int leftSize = tabFrame.getLeftTabCount();
        int rightSize = tabFrame.getRightTabCount();

        if (isDraggedOver(Alignment.NORTH)) topSize++;
        if (isDraggedOver(Alignment.SOUTH)) bottomSize++;
        if (isDraggedOver(Alignment.EAST)) rightSize++;
        if (isDraggedOver(Alignment.WEST)) leftSize++;

        ui.getDropComponentBottom().setSize(0, 0);
        ui.getDropComponentLeft().setSize(0, 0);
        ui.getDropComponentRight().setSize(0, 0);
        ui.getDropComponentTop().setSize(0, 0);

        topHeight = calculateMaxTabSize(Alignment.NORTH);
        bottomHeight = calculateMaxTabSize(Alignment.SOUTH);
        leftHeight = calculateMaxTabSize(Alignment.WEST);
        rightHeight = calculateMaxTabSize(Alignment.EAST);

        layoutTopTab(dim, topSize, leftSize, rightSize);
        layoutBottomTab(dim, bottomSize, leftSize, rightSize);
        layoutLeftTab(dim, leftSize);
        layoutRightTab(dim, rightSize);

        Component leftPane = ui.getLeftContainer();
        Component rightPane = ui.getRightContainer();
        Component topPane = ui.getTopContainer();
        Component bottomPane = ui.getBottomContainer();
        tabFrame.getContentPane().getComponent().setBounds(leftPane.getWidth(), topPane.getHeight(),
                                                           dim.width - leftPane.getWidth() - rightPane.getWidth(),
                                                           dim.height - topPane.getHeight() - bottomPane.getHeight());
    }

    protected void layoutTopTab(final Dimension dim, final int topSize, final int leftSize, final int rightSize) {
        Component topComp = tabFrame.getTopTabContainer();
        if (topSize > 0) {
            topComp.setBounds(0, 0, dim.width, tabFrame.getTabSize());
            layoutHorizontal(dim, Alignment.NORTH, Alignment.NORTH_EAST, 0, leftSize, rightSize, topHeight);
        } else if (draggedOver[getIndex(Alignment.NORTH)]) {
            int size = tabFrame.getTabSize();
            topComp.setBounds(0, 0, dim.width, size);
            if (ui.getDestIndex() >= -1) {
                layoutHorizontalDrop(Alignment.NORTH, leftSize, rightSize, size, 0);
            }
        } else {
            topComp.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutBottomTab(final Dimension dim, final int bottomSize, final int leftSize, final int rightSize) {
        Component bottomComp = tabFrame.getBottomTabContainer();
        if (bottomSize > 0) {
            bottomComp.setBounds(0, dim.height - bottomHeight, dim.width, bottomHeight);
            layoutHorizontal(dim, Alignment.SOUTH_WEST, Alignment.SOUTH, 1, leftSize, rightSize, bottomHeight);
        } else if (draggedOver[getIndex(Alignment.SOUTH)]) {
            int size = tabFrame.getTabSize();
            bottomComp.setBounds(0, dim.height - size, dim.width, size);
            if (ui.getDestIndex() >= -1) {
                layoutHorizontalDrop(Alignment.SOUTH_WEST, leftSize, rightSize, size, 1);
            }
        } else {
            bottomComp.setBounds(0, 0, 0, 0);
        }
    }

    protected void layoutHorizontalDrop(final Alignment left, final int leftSize, final int rightSize,
                                        final int size, final int yOff) {
        Alignment a = ui.getDestAlign();
        Dimension dropSize = ui.getDropSize();
        Component dropComp = ui.getDropComponent(left);
        Component tabComp = ui.getTabContainer(left);
        if (a == left) {
            int x = leftSize > 0 ? leftHeight : 0;
            dropComp.setBounds(x, yOff, dropSize.width, size);
        } else {
            int x = rightSize > 0 ? tabComp.getWidth() - rightHeight : tabComp.getWidth();
            dropComp.setBounds(x - dropSize.width, yOff, dropSize.width, size);
        }
    }

    protected void layoutHorizontal(final Dimension dim, final Alignment left, final Alignment right,
                                    final int yOff, final int leftSize, final int rightSize, final int tabHeight) {
        Point start = new Point(leftSize > 0 ? leftHeight : 0, yOff);
        int leftEnd = layoutTabArea(start, left, true, tabHeight - 1);
        start.x = rightSize > 0 ? dim.width - rightHeight : dim.width;
        int rightStart = layoutTabArea(start, right, false, tabHeight - 1);
        if (rightStart < leftEnd) {
            shift[getIndex(left)] = leftEnd - rightStart;
            shift(leftEnd - rightStart, right);
        } else {
            shift[getIndex(left)] = 0;
        }
    }

    protected void layoutLeftTab(final Dimension dim, final int leftSize) {
        Component leftPane = ui.getLeftContainer();
        Component topPane = tabFrame.getTopTabContainer();
        Component bottomPane = tabFrame.getBottomTabContainer();
        if (leftSize > 0 || draggedOver[getIndex(Alignment.WEST)]) {
            int size = leftSize > 0 ? leftHeight : tabFrame.getTabSize();
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            leftPane.setBounds(0, topPane.getHeight(), size, height + (height % 2));
            tabFrame.getLeftTabContainer().setPreferredSize(new Dimension(leftPane.getHeight(),
                                                                          leftPane.getWidth()));
            tabFrame.getLeftTabContainer().setSize(tabFrame.getLeftTabContainer().getPreferredSize());
            if (leftSize > 0) {
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

    protected void layoutRightTab(final Dimension dim, final int rightSize) {
        Component rightPane = ui.getRightContainer();
        Component topPane = tabFrame.getTopTabContainer();
        Component bottomPane = tabFrame.getBottomTabContainer();
        if (rightSize > 0 || draggedOver[getIndex(Alignment.EAST)]) {
            int size = rightSize > 0 ? rightHeight : tabFrame.getTabSize();
            int height = dim.height - topPane.getHeight() - bottomPane.getHeight();
            rightPane.setBounds(dim.width - rightHeight, topPane.getHeight(), size, height + (height % 2));
            tabFrame.getRightTabContainer().setPreferredSize(new Dimension(rightPane.getHeight(), rightPane.getWidth()));
            tabFrame.getRightTabContainer().setSize(tabFrame.getRightTabContainer().getPreferredSize());
            if (rightSize > 0) {
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

    public int getShift(@NotNull final Alignment a) {
        switch (a) {
            case NORTH:
            case EAST:
            case SOUTH_WEST:
            case NORTH_WEST:
                return 0;
        }
        return shift[getIndex(a)];
    }

    protected int getIndex(@NotNull final Alignment a) {
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

    protected int layoutTabArea(@NotNull final Point start, @NotNull final Alignment a,
                                final boolean forward, final int size) {
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

    public int getTabWidth(@NotNull final Component c) {
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
            max = Math.max(max, c.getComponent().getMaximumSize().height + 1);
        }
        for (TabFrameTab c : tabFrame.tabsForAlignment(tabFrame.getPeer(a))) {
            max = Math.max(max, c.getComponent().getMaximumSize().height + 1);
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
