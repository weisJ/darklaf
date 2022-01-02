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

import javax.swing.*;

/**
 * This class should be treated as a &quot;protected&quot; inner class. Instantiate it only within
 * subclasses of BasicTabbedPaneUI.
 */
public abstract class TabbedPaneLayout implements LayoutManager {

    protected final DarkTabbedPaneUIBridge ui;

    public TabbedPaneLayout(final DarkTabbedPaneUIBridge ui) {
        this.ui = ui;
    }

    @Override
    public void addLayoutComponent(final String name, final Component comp) {}

    @Override
    public void removeLayoutComponent(final Component comp) {}

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        return calculateSize(false);
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return calculateSize(true);
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("deprecation")
    public void layoutContainer(final Container parent) {
        /*
         * Some of the code in this method deals with changing the visibility of components to hide and show
         * the contents for the selected tab. This is older code that has since been duplicated in
         * JTabbedPane.fireStateChanged(), so as to allow visibility changes to happen sooner (see the note
         * there). This code remains for backward compatibility as there are some cases, such as subclasses
         * that don't fireStateChanged() where it may be used. Any changes here need to be kept in synch
         * with JTabbedPane.fireStateChanged().
         */
        ui.setRolloverTab(-1);

        int tabPlacement = ui.tabPane.getTabPlacement();
        Insets insets = ui.tabPane.getInsets();
        int selectedIndex = ui.tabPane.getSelectedIndex();
        Component visibleComponent = ui.getVisibleComponent();

        calculateLayoutInfo();

        Component selectedComponent = null;
        if (selectedIndex < 0) {
            if (visibleComponent != null) {
                // The last tab was removed, so remove the component
                ui.setVisibleComponent(null);
            }
        } else {
            selectedComponent = ui.tabPane.getComponentAt(selectedIndex);
        }
        int cx, cy, cw, ch;
        int totalTabWidth = 0;
        int totalTabHeight = 0;
        Insets contentInsets = ui.getContentBorderInsets(tabPlacement);

        boolean shouldChangeFocus = false;

        // In order to allow programs to use a single component
        // as the display for multiple tabs, we will not change
        // the visible compnent if the currently selected tab
        // has a null component. This is a bit dicey, as we don't
        // explicitly state we support this in the spec, but since
        // programs are now depending on this, we're making it work.
        //
        if (selectedComponent != null) {
            if (selectedComponent != visibleComponent && visibleComponent != null) {
                if (SwingUtilities.findFocusOwner(visibleComponent) != null) {
                    shouldChangeFocus = true;
                }
            }
            ui.setVisibleComponent(selectedComponent);
        }

        Rectangle bounds = ui.tabPane.getBounds();
        int numChildren = ui.tabPane.getComponentCount();

        if (numChildren > 0) {

            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    totalTabWidth = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
                    cx = insets.left + totalTabWidth + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    break;
                case SwingConstants.RIGHT:
                    totalTabWidth = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    break;
                case SwingConstants.BOTTOM:
                    totalTabHeight = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    break;
                case SwingConstants.TOP:
                default:
                    totalTabHeight = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + totalTabHeight + contentInsets.top;
            }

            cw = bounds.width - totalTabWidth - insets.left - insets.right - contentInsets.left - contentInsets.right;
            ch = bounds.height - totalTabHeight - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;

            for (int i = 0; i < numChildren; i++) {
                Component child = ui.tabPane.getComponent(i);
                if (child == ui.tabContainer) {

                    int tabContainerWidth = totalTabWidth == 0 ? bounds.width
                            : totalTabWidth + insets.left + insets.right + contentInsets.left + contentInsets.right;
                    int tabContainerHeight = totalTabHeight == 0 ? bounds.height
                            : totalTabHeight + insets.top + insets.bottom + contentInsets.top + contentInsets.bottom;

                    int tabContainerX = 0;
                    int tabContainerY = 0;
                    if (tabPlacement == SwingConstants.BOTTOM) {
                        tabContainerY = bounds.height - tabContainerHeight;
                    } else if (tabPlacement == SwingConstants.RIGHT) {
                        tabContainerX = bounds.width - tabContainerWidth;
                    }
                    child.setBounds(tabContainerX, tabContainerY, tabContainerWidth, tabContainerHeight);
                } else {
                    child.setBounds(cx, cy, cw, ch);
                }
            }
        }
        layoutTabComponents();
        if (shouldChangeFocus) {
            if (!ui.requestFocusForVisibleComponent()) {
                ui.tabPane.requestFocusInWindow();
            }
        }
    }

    /** Calculates the layout info. */
    public void calculateLayoutInfo() {
        int tabCount = ui.tabPane.getTabCount();
        ui.assureRectsCreated(tabCount);
        calculateTabRects(ui.tabPane.getTabPlacement(), tabCount);
        ui.isRunsDirty = false;
    }

    protected abstract void layoutTabComponents();

    /**
     * Calculate the tab rectangles.
     *
     * @param tabPlacement the tab placement
     * @param tabCount the tab count
     */
    protected void calculateTabRects(final int tabPlacement, final int tabCount) {
        FontMetrics metrics = ui.getFontMetrics();
        Dimension size = ui.tabPane.getSize();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);
        int fontHeight = metrics.getHeight();
        int selectedIndex = ui.tabPane.getSelectedIndex();
        int tabRunOverlay;
        int i, j;
        int x, y;
        int returnAt;
        boolean verticalTabRuns = tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT;
        boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();

        //
        // Calculate bounds within which a tab run must fit
        //
        switch (tabPlacement) {
            case SwingConstants.LEFT:
                ui.maxTabWidth = ui.calculateMaxTabWidth(tabPlacement);
                x = insets.left + tabAreaInsets.left;
                y = insets.top + tabAreaInsets.top;
                returnAt = size.height - (insets.bottom + tabAreaInsets.bottom);
                break;
            case SwingConstants.RIGHT:
                ui.maxTabWidth = ui.calculateMaxTabWidth(tabPlacement);
                x = size.width - insets.right - tabAreaInsets.right - ui.maxTabWidth;
                y = insets.top + tabAreaInsets.top;
                returnAt = size.height - (insets.bottom + tabAreaInsets.bottom);
                break;
            case SwingConstants.BOTTOM:
                ui.maxTabHeight = ui.calculateMaxTabHeight(tabPlacement);
                x = insets.left + tabAreaInsets.left;
                y = size.height - insets.bottom - tabAreaInsets.bottom - ui.maxTabHeight;
                returnAt = size.width - (insets.right + tabAreaInsets.right);
                break;
            case SwingConstants.TOP:
            default:
                ui.maxTabHeight = ui.calculateMaxTabHeight(tabPlacement);
                x = insets.left + tabAreaInsets.left;
                y = insets.top + tabAreaInsets.top;
                returnAt = size.width - (insets.right + tabAreaInsets.right);
                break;
        }

        tabRunOverlay = ui.getTabRunOverlay(tabPlacement);

        ui.runCount = 0;
        ui.selectedRun = -1;

        if (tabCount == 0) {
            return;
        }

        // Run through tabs and partition them into runs
        Rectangle rect;
        for (i = 0; i < tabCount; i++) {
            rect = ui.rects[i];

            if (!verticalTabRuns) {
                // Tabs on TOP or BOTTOM....
                if (i > 0) {
                    rect.x = ui.rects[i - 1].x + ui.rects[i - 1].width;
                } else {
                    ui.tabRuns[0] = 0;
                    ui.runCount = 1;
                    ui.maxTabWidth = 0;
                    rect.x = x;
                }
                rect.width = ui.calculateTabWidth(tabPlacement, i, metrics);
                ui.maxTabWidth = Math.max(ui.maxTabWidth, rect.width);

                // Never move a TAB down a run if it is in the first column.
                // Even if there isn't enough room, moving it to a fresh
                // line won't help.
                if (rect.x != x && rect.x + rect.width > returnAt) {
                    if (ui.runCount > ui.tabRuns.length - 1) {
                        ui.expandTabRunsArray();
                    }
                    ui.tabRuns[ui.runCount] = i;
                    ui.runCount++;
                    rect.x = x;
                }
                // Initialize y position in case there's just one run
                rect.y = y;
                rect.height = ui.maxTabHeight /* - 2 */;

            } else {
                // Tabs on LEFT or RIGHT...
                if (i > 0) {
                    rect.y = ui.rects[i - 1].y + ui.rects[i - 1].height;
                } else {
                    ui.tabRuns[0] = 0;
                    ui.runCount = 1;
                    ui.maxTabHeight = 0;
                    rect.y = y;
                }
                rect.height = ui.calculateTabHeight(tabPlacement, i, fontHeight);
                ui.maxTabHeight = Math.max(ui.maxTabHeight, rect.height);

                // Never move a TAB over a run if it is in the first run.
                // Even if there isn't enough room, moving it to a fresh
                // column won't help.
                if (rect.y != y && rect.y + rect.height > returnAt) {
                    if (ui.runCount > ui.tabRuns.length - 1) {
                        ui.expandTabRunsArray();
                    }
                    ui.tabRuns[ui.runCount] = i;
                    ui.runCount++;
                    rect.y = y;
                }
                // Initialize x position in case there's just one column
                rect.x = x;
                rect.width = ui.maxTabWidth /* - 2 */;
            }
            if (i == selectedIndex) {
                ui.selectedRun = ui.runCount - 1;
            }
        }

        if (ui.runCount > 1) {
            // Re-distribute tabs in case last run has leftover space
            normalizeTabRuns(tabPlacement, tabCount, verticalTabRuns ? y : x, returnAt);

            ui.selectedRun = ui.getRunForTab(tabCount, selectedIndex);

            // Rotate run array so that selected run is first
            if (ui.shouldRotateTabRuns(tabPlacement)) {
                rotateTabRuns(tabPlacement, ui.selectedRun);
            }
        }

        // Step through runs from back to front to calculate
        // tab y locations and to pad runs appropriately
        for (i = ui.runCount - 1; i >= 0; i--) {
            int start = ui.tabRuns[i];
            int next = ui.tabRuns[i == (ui.runCount - 1) ? 0 : i + 1];
            int end = next != 0 ? next - 1 : tabCount - 1;
            if (!verticalTabRuns) {
                for (j = start; j <= end; j++) {
                    rect = ui.rects[j];
                    rect.y = y;
                    rect.x += ui.getTabRunIndent(tabPlacement, i);
                }
                if (ui.shouldPadTabRun(tabPlacement, i)) {
                    padTabRun(tabPlacement, start, end, returnAt);
                }
                if (tabPlacement == SwingConstants.BOTTOM) {
                    y -= ui.maxTabHeight - tabRunOverlay;
                } else {
                    y += ui.maxTabHeight - tabRunOverlay;
                }
            } else {
                for (j = start; j <= end; j++) {
                    rect = ui.rects[j];
                    rect.x = x;
                    rect.y += ui.getTabRunIndent(tabPlacement, i);
                }
                if (ui.shouldPadTabRun(tabPlacement, i)) {
                    padTabRun(tabPlacement, start, end, returnAt);
                }
                if (tabPlacement == SwingConstants.RIGHT) {
                    x -= ui.maxTabWidth - tabRunOverlay;
                } else {
                    x += ui.maxTabWidth - tabRunOverlay;
                }
            }
        }

        centerTabs(tabPlacement, tabCount, returnAt);
        // Pad the selected tab so that it appears raised in front
        padSelectedTab(tabPlacement, selectedIndex);

        // if right to left and tab placement on the top or
        // the bottom, flip x positions and adjust by widths
        if (!leftToRight && !verticalTabRuns) {
            int rightMargin = size.width - (insets.right + tabAreaInsets.right);
            int leftMargin = insets.left + tabAreaInsets.left;
            for (i = 0; i < tabCount; i++) {
                ui.rects[i].x = rightMargin - ui.rects[i].x - ui.rects[i].width + leftMargin;
            }
        }
    }

    protected abstract void centerTabs(final int tabPlacement, final int tabCount, int returnAt);

    /**
     * Normalizes the tab runs.
     *
     * @param tabPlacement the tab placement
     * @param tabCount the tab count
     * @param start the start
     * @param max the max
     */
    protected void normalizeTabRuns(final int tabPlacement, final int tabCount, final int start, final int max) {
        boolean verticalTabRuns = tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT;
        int run = ui.runCount - 1;
        boolean keepAdjusting = true;
        double weight = 1.25;

        // At this point the tab runs are packed to fit as many
        // tabs as possible, which can leave the last run with a lot
        // of extra space (resulting in very fat tabs on the last run).
        // So we'll attempt to distribute this extra space more evenly
        // across the runs in order to make the runs look more consistent.
        //
        // Starting with the last run, determine whether the last tab in
        // the previous run would fit (generously) in this run; if so,
        // move tab to current run and shift tabs accordingly. Cycle
        // through remaining runs using the same algorithm.
        //
        while (keepAdjusting) {
            int last = ui.lastTabInRun(tabCount, run);
            int prevLast = ui.lastTabInRun(tabCount, run - 1);
            int end;
            int prevLastLen;

            if (!verticalTabRuns) {
                end = ui.rects[last].x + ui.rects[last].width;
                prevLastLen = (int) (ui.maxTabWidth * weight);
            } else {
                end = ui.rects[last].y + ui.rects[last].height;
                prevLastLen = (int) (ui.maxTabHeight * weight * 2);
            }

            // Check if the run has enough extra space to fit the last tab
            // from the previous row...
            if (max - end > prevLastLen) {

                // Insert tab from previous row and shift rest over
                ui.tabRuns[run] = prevLast;
                if (!verticalTabRuns) {
                    ui.rects[prevLast].x = start;
                } else {
                    ui.rects[prevLast].y = start;
                }
                for (int i = prevLast + 1; i <= last; i++) {
                    if (!verticalTabRuns) {
                        ui.rects[i].x = ui.rects[i - 1].x + ui.rects[i - 1].width;
                    } else {
                        ui.rects[i].y = ui.rects[i - 1].y + ui.rects[i - 1].height;
                    }
                }

            } else if (run == ui.runCount - 1) {
                // no more room left in last run, so we're done!
                keepAdjusting = false;
            }
            if (run - 1 > 0) {
                // check previous run next...
                run -= 1;
            } else {
                // check last run again...but require a higher ratio
                // of extraspace-to-tabsize because we don't want to
                // end up with too many tabs on the last run!
                run = ui.runCount - 1;
                weight += .25;
            }
        }
    }

    /**
     * Rotates the run-index array so that the selected run is run[0].
     *
     * @param tabPlacement the tab placement
     * @param selectedRun the selected run
     */
    protected void rotateTabRuns(final int tabPlacement, final int selectedRun) {
        for (int i = 0; i < selectedRun; i++) {
            int save = ui.tabRuns[0];
            if (ui.runCount - 1 >= 0) System.arraycopy(ui.tabRuns, 1, ui.tabRuns, 0, ui.runCount - 1);
            ui.tabRuns[ui.runCount - 1] = save;
        }
    }

    /**
     * Pads the tab run.
     *
     * @param tabPlacement the tab placement
     * @param start the start
     * @param end the end
     * @param max the max
     */
    protected void padTabRun(final int tabPlacement, final int start, final int end, final int max) {
        Rectangle lastRect = ui.rects[end];
        if (tabPlacement == SwingConstants.TOP || tabPlacement == SwingConstants.BOTTOM) {
            int runWidth = (lastRect.x + lastRect.width) - ui.rects[start].x;
            int deltaWidth = max - (lastRect.x + lastRect.width);
            float factor = (float) deltaWidth / (float) runWidth;

            for (int j = start; j <= end; j++) {
                Rectangle pastRect = ui.rects[j];
                if (j > start) {
                    pastRect.x = ui.rects[j - 1].x + ui.rects[j - 1].width;
                }
                pastRect.width += Math.round((float) pastRect.width * factor);
            }
            lastRect.width = max - lastRect.x;
        } else {
            int runHeight = (lastRect.y + lastRect.height) - ui.rects[start].y;
            int deltaHeight = max - (lastRect.y + lastRect.height);
            float factor = (float) deltaHeight / (float) runHeight;

            for (int j = start; j <= end; j++) {
                Rectangle pastRect = ui.rects[j];
                if (j > start) {
                    pastRect.y = ui.rects[j - 1].y + ui.rects[j - 1].height;
                }
                pastRect.height += Math.round((float) pastRect.height * factor);
            }
            lastRect.height = max - lastRect.y;
        }
    }

    /**
     * Pads selected tab.
     *
     * @param tabPlacement the tab placement
     * @param selectedIndex the selected index
     */
    protected void padSelectedTab(final int tabPlacement, final int selectedIndex) {

        if (selectedIndex >= 0) {
            Rectangle selRect = ui.rects[selectedIndex];
            Insets padInsets = ui.getSelectedTabPadInsets(tabPlacement);
            selRect.x -= padInsets.left;
            selRect.width += padInsets.left + padInsets.right;
            selRect.y -= padInsets.top;
            selRect.height += padInsets.top + padInsets.bottom;

            if (!ui.scrollableTabLayoutEnabled()) { // WRAP_TAB_LAYOUT
                // do not expand selected tab more then necessary
                Dimension size = ui.tabPane.getSize();
                Insets insets = ui.tabPane.getInsets();

                if ((tabPlacement == SwingConstants.LEFT) || (tabPlacement == SwingConstants.RIGHT)) {
                    int top = insets.top - selRect.y;
                    if (top > 0) {
                        selRect.y += top;
                        selRect.height -= top;
                    }
                    int bottom = selRect.y + selRect.height + insets.bottom - size.height;
                    if (bottom > 0) {
                        selRect.height -= bottom;
                    }
                } else {
                    int left = insets.left - selRect.x;
                    if (left > 0) {
                        selRect.x += left;
                        selRect.width -= left;
                    }
                    int right = selRect.x + selRect.width + insets.right - size.width;
                    if (right > 0) {
                        selRect.width -= right;
                    }
                }
            }
        }
    }

    /**
     * Returns the calculated size.
     *
     * @param minimum use the minimum size or preferred size
     * @return the calculated size
     */
    protected Dimension calculateSize(final boolean minimum) {
        int tabPlacement = ui.tabPane.getTabPlacement();
        Insets insets = ui.tabPane.getInsets();
        Insets contentInsets = ui.getContentBorderInsets(tabPlacement);
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);

        int height = 0;
        int width = 0;
        int cWidth = 0;
        int cHeight = 0;

        // Determine minimum size required to display largest
        // child in each dimension
        //
        for (int i = 0; i < ui.tabPane.getTabCount(); i++) {
            Component component = ui.tabPane.getComponentAt(i);
            if (component != null) {
                Dimension size = minimum ? component.getMinimumSize() : component.getPreferredSize();

                if (size != null) {
                    cHeight = Math.max(size.height, cHeight);
                    cWidth = Math.max(size.width, cWidth);
                }
            }
        }
        // Add content border insets to minimum size
        width += cWidth;
        height += cHeight;
        int tabExtent;

        // Calculate how much space the tabs will need, based on the
        // minimum size required to display largest child + content border
        //
        switch (tabPlacement) {
            case SwingConstants.LEFT:
            case SwingConstants.RIGHT:
                height = Math.max(height, ui.calculateMaxTabHeight(tabPlacement));
                tabExtent = preferredTabAreaWidth(tabPlacement, height - tabAreaInsets.top - tabAreaInsets.bottom);
                width += tabExtent;
                break;
            case SwingConstants.TOP:
            case SwingConstants.BOTTOM:
            default:
                width = Math.max(width, ui.calculateMaxTabWidth(tabPlacement));
                tabExtent = preferredTabAreaHeight(tabPlacement, width - tabAreaInsets.left - tabAreaInsets.right);
                height += tabExtent;
        }
        return new Dimension(width + insets.left + insets.right + contentInsets.left + contentInsets.right,
                height + insets.bottom + insets.top + contentInsets.top + contentInsets.bottom);
    }

    /**
     * Returns the preferred tab area width.
     *
     * @param tabPlacement the tab placement
     * @param height the height
     * @return the preferred tab area widty
     */
    protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
        FontMetrics metrics = ui.getFontMetrics();
        int tabCount = ui.tabPane.getTabCount();
        int total = 0;
        if (tabCount > 0) {
            int columns = 1;
            int y = 0;
            int fontHeight = metrics.getHeight();

            ui.maxTabWidth = ui.calculateMaxTabWidth(tabPlacement);

            for (int i = 0; i < tabCount; i++) {
                int tabHeight = ui.calculateTabHeight(tabPlacement, i, fontHeight);

                if (y != 0 && y + tabHeight > height) {
                    columns++;
                    y = 0;
                }
                y += tabHeight;
            }
            total = ui.calculateTabAreaWidth(tabPlacement, columns, ui.maxTabWidth);
        }
        return total;
    }

    /**
     * Returns the preferred tab area height.
     *
     * @param tabPlacement the tab placement
     * @param width the width
     * @return the preferred tab area height
     */
    protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
        FontMetrics metrics = ui.getFontMetrics();
        int tabCount = ui.tabPane.getTabCount();
        int total = 0;
        if (tabCount > 0) {
            int rows = 1;
            int x = 0;

            int maxTabHeight = ui.calculateMaxTabHeight(tabPlacement);

            for (int i = 0; i < tabCount; i++) {
                int tabWidth = ui.calculateTabWidth(tabPlacement, i, metrics);

                if (x != 0 && x + tabWidth > width) {
                    rows++;
                    x = 0;
                }
                x += tabWidth;
            }
            total = ui.calculateTabAreaHeight(tabPlacement, rows, maxTabHeight);
        }
        return total;
    }
}
