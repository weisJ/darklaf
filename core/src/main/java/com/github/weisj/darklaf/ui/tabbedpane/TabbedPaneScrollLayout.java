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

import javax.swing.*;
import java.awt.*;

abstract class TabbedPaneScrollLayout extends TabbedPaneLayout {


    public TabbedPaneScrollLayout(final DarkTabbedPaneUIBridge ui) {
        super(ui);
    }

    protected void calculateTabRects(final int tabPlacement, final int tabCount) {
        FontMetrics metrics = ui.getFontMetrics();
        Dimension size = ui.tabPane.getSize();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);
        int fontHeight = metrics.getHeight();
        int selectedIndex = ui.tabPane.getSelectedIndex();
        int i;
        boolean verticalTabRuns = (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT);
        boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();
        int x = tabAreaInsets.left;
        int y = tabAreaInsets.top;
        int totalWidth = 0;
        int totalHeight = 0;

        //
        // Calculate bounds within which a tab run must fit
        //
        switch (tabPlacement) {
            case SwingConstants.LEFT:
            case SwingConstants.RIGHT:
                ui.maxTabWidth = ui.calculateMaxTabWidth(tabPlacement);
                break;
            case SwingConstants.BOTTOM:
            case SwingConstants.TOP:
            default:
                ui.maxTabHeight = ui.calculateMaxTabHeight(tabPlacement);
        }

        ui.runCount = 0;
        ui.selectedRun = -1;

        if (tabCount == 0) {
            return;
        }

        ui.selectedRun = 0;
        ui.runCount = 1;

        // Run through tabs and lay them out in a single run
        Rectangle rect;
        for (i = 0; i < tabCount; i++) {
            rect = ui.rects[i];

            if (!verticalTabRuns) {
                // Tabs on TOP or BOTTOM....
                if (i > 0) {
                    rect.x = ui.rects[i - 1].x + ui.rects[i - 1].width;
                } else {
                    ui.tabRuns[0] = 0;
                    ui.maxTabWidth = 0;
                    totalHeight += ui.maxTabHeight;
                    rect.x = x;
                }
                rect.width = ui.calculateTabWidth(tabPlacement, i, metrics);
                totalWidth = rect.x + rect.width;
                ui.maxTabWidth = Math.max(ui.maxTabWidth, rect.width);

                rect.y = y;
                rect.height = ui.maxTabHeight/* - 2*/;

            } else {
                // Tabs on LEFT or RIGHT...
                if (i > 0) {
                    rect.y = ui.rects[i - 1].y + ui.rects[i - 1].height;
                } else {
                    ui.tabRuns[0] = 0;
                    ui.maxTabHeight = 0;
                    totalWidth = ui.maxTabWidth;
                    rect.y = y;
                }
                rect.height = ui.calculateTabHeight(tabPlacement, i, fontHeight);
                totalHeight = rect.y + rect.height;
                ui.maxTabHeight = Math.max(ui.maxTabHeight, rect.height);

                rect.x = x;
                rect.width = ui.maxTabWidth/* - 2*/;

            }
        }

        if (ui.tabsOverlapBorder) {
            // Pad the selected tab so that it appears raised in front
            padSelectedTab(tabPlacement, selectedIndex);
        }

        // if right to left and tab placement on the top or
        // the bottom, flip x positions and adjust by widths
        if (!leftToRight && !verticalTabRuns) {
            int rightMargin = size.width
                    - (insets.right + tabAreaInsets.right);
            for (i = 0; i < tabCount; i++) {
                ui.rects[i].x = rightMargin - ui.rects[i].x - ui.rects[i].width;
            }
        }
        ui.tabScroller.tabPanel.setPreferredSize(new Dimension(totalWidth, totalHeight));
    }

    protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
        return ui.calculateMaxTabWidth(tabPlacement);
    }

    protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
        return ui.calculateMaxTabHeight(tabPlacement);
    }
}
