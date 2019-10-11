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
package com.weis.darklaf.ui.tabbedpane;

import javax.swing.*;
import java.awt.*;

class TabbedPaneScrollLayout extends TabbedPaneLayout {


    public TabbedPaneScrollLayout(final DarkTabbedPaneUIBridge ui) {
        super(ui);
    }

    protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
        return ui.calculateMaxTabHeight(tabPlacement);
    }

    protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
        return ui.calculateMaxTabWidth(tabPlacement);
    }

    @SuppressWarnings("deprecation")
    public void layoutContainer(final Container parent) {
        /* Some of the code in this method deals with changing the
         * visibility of components to hide and show the contents for the
         * selected tab. This is older code that has since been duplicated
         * in JTabbedPane.fireStateChanged(), so as to allow visibility
         * changes to happen sooner (see the note there). This code remains
         * for backward compatibility as there are some cases, such as
         * subclasses that don't fireStateChanged() where it may be used.
         * Any changes here need to be kept in synch with
         * JTabbedPane.fireStateChanged().
         */

        ui.setRolloverTab(-1);

        int tabPlacement = ui.tabPane.getTabPlacement();
        int tabCount = ui.tabPane.getTabCount();
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

        if (ui.tabPane.getTabCount() == 0) {
            ui.tabScroller.croppedEdge.resetParams();
            ui.tabScroller.scrollForwardButton.setVisible(false);
            ui.tabScroller.scrollBackwardButton.setVisible(false);
            return;
        }

        boolean shouldChangeFocus = false;

        // In order to allow programs to use a single component
        // as the display for multiple tabs, we will not change
        // the visible compnent if the currently selected tab
        // has a null component.  This is a bit dicey, as we don't
        // explicitly state we support this in the spec, but since
        // programs are now depending on this, we're making it work.
        //
        if (selectedComponent != null) {
            if (selectedComponent != visibleComponent &&
                    visibleComponent != null) {
                if (SwingUtilities.findFocusOwner(visibleComponent) != null) {
                    shouldChangeFocus = true;
                }
            }
            ui.setVisibleComponent(selectedComponent);
        }
        int tx, ty, tw, th; // tab area bounds
        int cx, cy, cw, ch; // content area bounds
        Insets contentInsets = ui.getContentBorderInsets(tabPlacement);
        Rectangle bounds = ui.tabPane.getBounds();
        int numChildren = ui.tabPane.getComponentCount();

        if (numChildren > 0) {
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    // calculate tab area bounds
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
                    th = bounds.height - insets.top - insets.bottom;
                    tx = insets.left;
                    ty = insets.top;

                    // calculate content area bounds
                    cx = tx + tw + contentInsets.left;
                    cy = ty + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - tw -
                            contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom -
                            contentInsets.top - contentInsets.bottom;
                    break;
                case SwingConstants.RIGHT:
                    // calculate tab area bounds
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
                    th = bounds.height - insets.top - insets.bottom;
                    tx = bounds.width - insets.right - tw;
                    ty = insets.top;

                    // calculate content area bounds
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - tw -
                            contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom -
                            contentInsets.top - contentInsets.bottom;
                    break;
                case SwingConstants.BOTTOM:
                    // calculate tab area bounds
                    tw = bounds.width - insets.left - insets.right;
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    tx = insets.left;
                    ty = bounds.height - insets.bottom - th;

                    // calculate content area bounds
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right -
                            contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - th -
                            contentInsets.top - contentInsets.bottom;
                    break;
                case SwingConstants.TOP:
                default:
                    // calculate tab area bounds
                    tw = bounds.width - insets.left - insets.right;
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    tx = insets.left;
                    ty = insets.top;

                    // calculate content area bounds
                    cx = tx + contentInsets.left;
                    cy = ty + th + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right -
                            contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - th -
                            contentInsets.top - contentInsets.bottom;
            }

            for (int i = 0; i < numChildren; i++) {
                Component child = ui.tabPane.getComponent(i);

                if (ui.tabScroller != null && child == ui.tabScroller.viewport) {
                    JViewport viewport = (JViewport) child;
                    Rectangle viewRect = viewport.getViewRect();
                    int vw = tw;
                    int vh = th;
                    Dimension butSize = ui.tabScroller.scrollForwardButton.getPreferredSize();
                    switch (tabPlacement) {
                        case SwingConstants.LEFT:
                        case SwingConstants.RIGHT:
                            int totalTabHeight = ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height;
                            if (totalTabHeight > th) {
                                // Allow space for scrollbuttons
                                vh = (th > 2 * butSize.height) ? th - 2 * butSize.height : 0;
                                if (totalTabHeight - viewRect.y <= vh) {
                                    // Scrolled to the end, so ensure the viewport size is
                                    // such that the scroll offset aligns with a tab
                                    vh = totalTabHeight - viewRect.y;
                                }
                            }
                            break;
                        case SwingConstants.BOTTOM:
                        case SwingConstants.TOP:
                        default:
                            int totalTabWidth = ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width;
                            if (totalTabWidth > tw) {
                                // Need to allow space for scrollbuttons
                                vw = (tw > 2 * butSize.width) ? tw - 2 * butSize.width : 0;
                                if (totalTabWidth - viewRect.x <= vw) {
                                    // Scrolled to the end, so ensure the viewport size is
                                    // such that the scroll offset aligns with a tab
                                    vw = totalTabWidth - viewRect.x;
                                }
                            }
                    }
                    child.setBounds(tx, ty, vw, vh);

                } else if (ui.tabScroller != null &&
                        (child == ui.tabScroller.scrollForwardButton ||
                                child == ui.tabScroller.scrollBackwardButton)) {
                    Dimension bsize = child.getPreferredSize();
                    int bx = 0;
                    int by = 0;
                    int bw = bsize.width;
                    int bh = bsize.height;
                    boolean visible = false;

                    switch (tabPlacement) {
                        case SwingConstants.LEFT:
                        case SwingConstants.RIGHT:
                            int totalTabHeight = ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height;
                            if (totalTabHeight > th) {
                                visible = true;
                                bx = (tabPlacement == SwingConstants.LEFT ? tx + tw - bsize.width : tx);
                                by = (child == ui.tabScroller.scrollForwardButton) ?
                                     bounds.height - insets.bottom - bsize.height :
                                     bounds.height - insets.bottom - 2 * bsize.height;
                            }
                            break;

                        case SwingConstants.BOTTOM:
                        case SwingConstants.TOP:
                        default:
                            int totalTabWidth = ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width;

                            if (totalTabWidth > tw) {
                                visible = true;
                                bx = (child == ui.tabScroller.scrollForwardButton) ?
                                     bounds.width - insets.left - bsize.width :
                                     bounds.width - insets.left - 2 * bsize.width;
                                by = (tabPlacement == SwingConstants.TOP ? ty + th - bsize.height : ty);
                            }
                    }
                    child.setVisible(visible);
                    if (visible) {
                        child.setBounds(bx, by, bw, bh);
                    }

                } else {
                    // All content children...
                    child.setBounds(cx, cy, cw, ch);
                }
            }
            super.layoutTabComponents();
            layoutCroppedEdge();
            if (shouldChangeFocus) {
                if (!ui.requestFocusForVisibleComponent()) {
                    ui.tabPane.requestFocus();
                }
            }
        }
    }

    protected void layoutCroppedEdge() {
        ui.tabScroller.croppedEdge.resetParams();
        Rectangle viewRect = ui.tabScroller.viewport.getViewRect();
        int cropline;
        for (int i = 0; i < ui.rects.length; i++) {
            Rectangle tabRect = ui.rects[i];
            switch (ui.tabPane.getTabPlacement()) {
                case SwingConstants.LEFT:
                case SwingConstants.RIGHT:
                    cropline = viewRect.y + viewRect.height;
                    if ((tabRect.y < cropline) && (tabRect.y + tabRect.height > cropline)) {
                        ui.tabScroller.croppedEdge.setParams(i, cropline - tabRect.y - 1,
                                                             -ui.currentTabAreaInsets.left, 0);
                    }
                    break;
                case SwingConstants.TOP:
                case SwingConstants.BOTTOM:
                default:
                    cropline = viewRect.x + viewRect.width;
                    if ((tabRect.x < cropline - 1) && (tabRect.x + tabRect.width > cropline)) {
                        ui.tabScroller.croppedEdge.setParams(i, cropline - tabRect.x - 1,
                                                             0, -ui.currentTabAreaInsets.top);
                    }
            }
        }
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
        ui.tabScroller.tabPanel.invalidate();
    }
}
