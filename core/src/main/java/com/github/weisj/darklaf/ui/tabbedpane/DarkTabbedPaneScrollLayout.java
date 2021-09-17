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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;
import java.util.function.Function;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTabbedPaneScrollLayout extends TabbedPaneScrollLayout {

    private final DarkTabbedPaneUI tabbedPaneUI;

    public DarkTabbedPaneScrollLayout(final DarkTabbedPaneUI tabbedPaneUI) {
        super(tabbedPaneUI);
        this.tabbedPaneUI = tabbedPaneUI;
    }

    @Override
    public void layoutContainer(final Container parent) {
        int tabPlacement = tabbedPaneUI.tabPane.getTabPlacement();
        Insets insets = tabbedPaneUI.tabPane.getInsets();
        Insets tabAreaInsets = tabbedPaneUI.getTabAreaInsets(tabPlacement);
        int selectedIndex = tabbedPaneUI.tabPane.getSelectedIndex();
        Component visibleComponent = tabbedPaneUI.getVisibleComponent();

        calculateLayoutInfo();

        Component selectedComponent = null;
        if (selectedIndex < 0) {
            if (visibleComponent != null) {
                // The last tab was removed, so remove the component
                tabbedPaneUI.setVisibleComponent(null);
            }
        } else {
            selectedComponent = tabbedPaneUI.tabPane.getComponentAt(selectedIndex);
        }

        if (tabbedPaneUI.tabPane.getTabCount() == 0) {
            tabbedPaneUI.scrollableTabSupport.hideMoreTabsButton();
            return;
        }

        boolean shouldChangeFocus = false;

        // In order to allow programs to use a single component
        // as the display for multiple tabs, we will not change
        // the visible component if the currently selected tab
        // has a null component. This is a bit dicey, as we don't
        // explicitly state we support this in the spec, but since
        // programs are now depending on this, we're making it work.
        //
        if (selectedComponent != null) {
            if (selectedComponent != visibleComponent && visibleComponent != null) {

                Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                if (owner != null && SwingUtilities.isDescendingFrom(owner, visibleComponent)) {
                    shouldChangeFocus = true;
                }
            }
            tabbedPaneUI.setVisibleComponent(selectedComponent);
        }
        int tx, ty, tw, th; // tab area bounds
        int cx, cy, cw, ch; // content area bounds
        Insets contentInsets = tabbedPaneUI.getContentBorderInsets(tabPlacement);
        Rectangle bounds = tabbedPaneUI.tabPane.getBounds();
        int numChildren = tabbedPaneUI.tabPane.getComponentCount();

        if (numChildren > 0) {
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    tw = tabbedPaneUI.calculateTabAreaWidth(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabWidth);
                    th = bounds.height - insets.top - insets.bottom - tabAreaInsets.top - tabAreaInsets.bottom;
                    tx = insets.left + tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    cx = insets.left + tw + contentInsets.left + tabAreaInsets.left + tabAreaInsets.right;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - tw - contentInsets.left - contentInsets.right
                            - tabAreaInsets.left - tabAreaInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                    tw -= tabAreaInsets.left + tabAreaInsets.right;
                    break;
                case SwingConstants.RIGHT:
                    tw = tabbedPaneUI.calculateTabAreaWidth(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabWidth);
                    th = bounds.height - insets.top - insets.bottom - tabAreaInsets.top - tabAreaInsets.bottom;
                    tx = bounds.width - insets.right - tw + tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - tw - contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                    tw -= tabAreaInsets.left + tabAreaInsets.right;
                    break;
                case SwingConstants.BOTTOM:
                    tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                    th = tabbedPaneUI.calculateTabAreaHeight(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabHeight);
                    tx = insets.left + tabAreaInsets.left;
                    ty = bounds.height - insets.bottom - th + tabAreaInsets.top;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - th - contentInsets.top - contentInsets.bottom;
                    th -= tabAreaInsets.top + tabAreaInsets.bottom;
                    break;
                default:
                    tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                    th = tabbedPaneUI.calculateTabAreaHeight(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabHeight);
                    tx = insets.left + tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + th + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - th - contentInsets.top - contentInsets.bottom;
                    th -= tabAreaInsets.top + tabAreaInsets.bottom;
                    break;
            }
            JComponent moreTabs = tabbedPaneUI.scrollableTabSupport.moreTabsButton;
            JComponent newTab = tabbedPaneUI.scrollableTabSupport.newTabButton;

            for (int i = 0; i < numChildren; i++) {
                Component child = tabbedPaneUI.tabPane.getComponent(i);

                if (tabbedPaneUI.tabScroller != null && child == tabbedPaneUI.tabScroller.viewport) {
                    int vw = tw;
                    int vh = th;
                    Dimension butSize = moreTabs.isVisible() ? moreTabs.getPreferredSize() : new Dimension(0, 0);
                    boolean showNewTabButton = newTab.isVisible() && newTab.getParent() == tabbedPaneUI.tabPane;
                    Dimension butSize2 = showNewTabButton ? newTab.getPreferredSize() : new Dimension(0, 0);
                    boolean leftToRight = tabbedPaneUI.tabPane.getComponentOrientation().isLeftToRight();
                    if (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT) {
                        vh = th - butSize.height - butSize2.height;
                        moreTabs.setBounds(tx, ty + vh + butSize2.height, tabbedPaneUI.maxTabWidth, butSize.height);
                        if (showNewTabButton) {
                            newTab.setBounds(tx, ty + vh, tabbedPaneUI.maxTabWidth, butSize2.height);
                        }
                    } else {
                        if (leftToRight) {
                            vw = tw - butSize.width - butSize2.width;
                            moreTabs.setBounds(tx + vw + butSize2.width, ty, butSize.width, tabbedPaneUI.maxTabHeight);
                            if (showNewTabButton) {
                                newTab.setBounds(tx + vw, ty, butSize2.width, tabbedPaneUI.maxTabHeight);
                            }
                        } else {
                            vw = tw - butSize.width - butSize2.width;
                            moreTabs.setBounds(tx, ty, butSize.width, tabbedPaneUI.maxTabHeight);
                            if (showNewTabButton) {
                                newTab.setBounds(tx + butSize.width, ty, butSize2.width, tabbedPaneUI.maxTabHeight);
                            }
                            tx += butSize.width + butSize2.width;
                        }
                    }
                    child.setBounds(tx, ty, vw, vh);
                } else {
                    int tabHeight = tabbedPaneUI.maxTabHeight + tabAreaInsets.top + tabAreaInsets.bottom;
                    int tabWidth = tabbedPaneUI.maxTabWidth + tabAreaInsets.left + tabAreaInsets.right;
                    int compHeight = ch;
                    int compY = cy;
                    if (tabbedPaneUI.northComp != null) {
                        int nh = tabbedPaneUI.northComp.getPreferredSize().height;
                        compY -= nh;
                        compHeight += nh;
                    }
                    if (tabbedPaneUI.southComp != null) {
                        compHeight += tabbedPaneUI.southComp.getPreferredSize().height;
                    }
                    if (child == tabbedPaneUI.leadingComp && tabbedPaneUI.leadingComp != null) {
                        tabbedPaneUI.layoutLeadingComponent(child, tabWidth, tabHeight, insets, tx, ty, tabPlacement);
                    } else if (child == tabbedPaneUI.trailingComp && tabbedPaneUI.trailingComp != null) {
                        tabbedPaneUI.layoutTrailingComponent(child, tabWidth, tabHeight, insets, tx, ty, tw, th,
                                tabPlacement);
                    } else if (child == tabbedPaneUI.northComp && tabbedPaneUI.northComp != null) {
                        tabbedPaneUI.northComp.setBounds(cx, cy - tabbedPaneUI.northComp.getPreferredSize().height, cw,
                                tabbedPaneUI.northComp.getPreferredSize().height);
                    } else if (child == tabbedPaneUI.southComp && tabbedPaneUI.southComp != null) {
                        tabbedPaneUI.southComp.setBounds(cx, cy + ch, cw,
                                tabbedPaneUI.southComp.getPreferredSize().height);
                    } else if (child == tabbedPaneUI.eastComp && tabbedPaneUI.eastComp != null) {
                        tabbedPaneUI.eastComp.setBounds(cx + cw, compY, tabbedPaneUI.eastComp.getPreferredSize().width,
                                compHeight);
                    } else if (child == tabbedPaneUI.westComp && tabbedPaneUI.westComp != null) {
                        tabbedPaneUI.westComp.setBounds(cx - tabbedPaneUI.westComp.getPreferredSize().width, compY,
                                tabbedPaneUI.westComp.getPreferredSize().width, compHeight);
                    } else if (child != moreTabs && child != newTab && child != null) {
                        child.setBounds(cx, cy, cw, ch);
                    }
                }
            }
            layoutTabComponents();
            if (shouldChangeFocus) {
                if (!tabbedPaneUI.requestFocusForVisibleComponent()) {
                    tabbedPaneUI.tabPane.requestFocusInWindow();
                }
            }
        }
    }

    @Override
    protected void layoutTabComponents() {
        tabbedPaneUI.layoutTabComponents();
    }

    @Override
    protected Dimension calculateSize(final boolean minimum) {
        int tabPlacement = tabbedPaneUI.tabPane.getTabPlacement();
        Insets insets = tabbedPaneUI.tabPane.getInsets();
        Insets contentInsets = tabbedPaneUI.getContentBorderInsets(tabPlacement);
        Insets tabAreaInsets = tabbedPaneUI.getTabAreaInsets(tabPlacement);

        int height = 0;
        int width = 0;
        int cWidth = 0;
        int cHeight = 0;

        // Determine minimum size required to display largest
        // child in each dimension
        //
        for (int i = 0; i < tabbedPaneUI.tabPane.getTabCount(); i++) {
            Component component = tabbedPaneUI.tabPane.getComponentAt(i);
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
        if (tabbedPaneUI.tabPane.getTabCount() > 0) {
            if (!tabbedPaneUI.isHorizontalTabPlacement()) {
                int tabHeight = tabbedPaneUI.calculateTabHeight(tabPlacement, tabbedPaneUI.tabPane.getSelectedIndex(),
                        tabbedPaneUI.getFontMetrics().getHeight());
                if (tabbedPaneUI.scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabHeight += tabbedPaneUI.scrollableTabSupport.moreTabsButton.getPreferredSize().height;
                }
                height = Math.max(height, tabHeight);
                tabExtent = preferredTabAreaWidth(tabPlacement, height - tabAreaInsets.top - tabAreaInsets.bottom);
                width += tabExtent;
            } else {
                int tabWidth = tabbedPaneUI.calculateTabWidth(tabPlacement, tabbedPaneUI.tabPane.getSelectedIndex(),
                        tabbedPaneUI.getFontMetrics());
                if (tabbedPaneUI.scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabWidth += tabbedPaneUI.scrollableTabSupport.moreTabsButton.getPreferredSize().width;
                }
                width = Math.max(width, tabWidth);
                tabExtent = preferredTabAreaHeight(tabPlacement, width - tabAreaInsets.left - tabAreaInsets.right);
                height += tabExtent;
            }
        }
        return new Dimension(width + insets.left + insets.right + contentInsets.left + contentInsets.right,
                height + insets.bottom + insets.top + contentInsets.top + contentInsets.bottom);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    @Override
    protected void calculateTabRects(final int tabPlacement, final int tabCount) {
        if (tabbedPaneUI.scrollShiftX != 0 || tabbedPaneUI.scrollShiftY != 0) {
            // We don't want to recalculate the tab rects because while scrolling all additional position
            // calculates shouldn't be taken into consideration.
            // This method will be called if there are custom tab components. If they are shifted the parent is
            // forced
            // to renew the layout of its component. We know that during scrolling the tab rects are already
            // correctly
            // calculated (with the shift applied) so there is no need to recalculate here.
            return;
        }
        FontMetrics metrics = tabbedPaneUI.getFontMetrics();
        Dimension size = tabbedPaneUI.tabPane.getSize();
        Insets insets = tabbedPaneUI.tabPane.getInsets();
        Insets tabAreaInsets = tabbedPaneUI.getTabAreaInsets(tabPlacement);
        boolean verticalTabRuns = !tabbedPaneUI.isHorizontalTabPlacement();
        boolean leftToRight = tabbedPaneUI.tabPane.getComponentOrientation().isLeftToRight();

        if (!tabbedPaneUI.isHorizontalTabPlacement()) {
            tabbedPaneUI.maxTabWidth = tabbedPaneUI.calculateMaxTabWidth(tabPlacement);
        } else {
            tabbedPaneUI.maxTabHeight = tabbedPaneUI.calculateMaxTabHeight(tabPlacement);
        }

        tabbedPaneUI.runCount = 0;
        tabbedPaneUI.selectedRun = -1;

        if (tabCount == 0) return;

        tabbedPaneUI.selectedRun = 0;
        tabbedPaneUI.runCount = 1;

        int tabAreaWidth = Math.max(tabbedPaneUI.calculateTabAreaWidth(tabPlacement, 0, 0), tabbedPaneUI.maxTabWidth);
        int tabAreaHeight =
                Math.max(tabbedPaneUI.calculateTabAreaHeight(tabPlacement, 0, 0), tabbedPaneUI.maxTabHeight);
        Rectangle tabBounds = new Rectangle(0, 0, tabAreaWidth, tabAreaHeight);
        for (int i = 0; i < tabCount; i++) {
            calculateRect(i, tabBounds, metrics, verticalTabRuns, tabPlacement);
        }

        JComponent tabsButton = tabbedPaneUI.scrollableTabSupport.moreTabsButton;
        Rectangle selectedBounds =
                tabbedPaneUI.tabPane.getSelectedIndex() > 0
                        ? new Rectangle(tabbedPaneUI.rects[tabbedPaneUI.tabPane.getSelectedIndex()])
                        : new Rectangle(0, 0, 0, 0);
        if (!verticalTabRuns) {
            int rightMargin = size.width - (insets.right + tabAreaInsets.right + insets.left + tabAreaInsets.left);
            Point p = getMargins(tabPlacement);
            int leftMargin = p.x;
            int returnAt = p.y;

            tabbedPaneUI.currentShiftXTmp = tabbedPaneUI.currentShiftX;
            shiftTabsX(0, leftMargin, returnAt, tabCount, false);
            if (!(tabbedPaneUI.minVisible == tabbedPaneUI.maxVisible
                    && tabbedPaneUI.minVisible == tabbedPaneUI.tabPane.getSelectedIndex())) {
                selectedBounds.x += tabbedPaneUI.currentShiftXTmp;
                shiftBoundsToVisibleX(selectedBounds, leftMargin, returnAt, tabCount);
            }
            restoreHiddenTabsX(leftMargin, returnAt, tabCount);

            if (tabbedPaneUI.minVisible == 0 && tabbedPaneUI.maxVisible == tabCount - 1
                    && PropertyUtil.getBooleanProperty(tabbedPaneUI.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)) {
                adjustForCenterX(leftMargin, returnAt, tabCount);
            }

            if (tabsButton.isVisible() && tabbedPaneUI.tabPane.getSelectedIndex() < tabbedPaneUI.maxVisible) {
                // Shift again. Hiding the the tab button might reveal the last tab.
                // Only do this if the last visible tab is not currently selected.
                // Otherwise the selected tab forces the whole tab area the jump by the width of the tab
                // button.
                int margin = returnAt + tabsButton.getPreferredSize().width;
                shiftTabsX(0, leftMargin, margin, tabCount, false);
                if (tabbedPaneUI.minVisible > 0 || tabbedPaneUI.maxVisible < tabCount - 1) {
                    // Tab button is still visible but may hide a further tab. restore visible bounds.
                    shiftTabsX(0, leftMargin, returnAt, tabCount, false);
                }
            }
            adjustForDropX(leftMargin, returnAt, tabCount);

            layoutMoreTabsButton(tabCount);

            commitShiftX(tabbedPaneUI.currentShiftXTmp, tabCount);
            tabbedPaneUI.currentShiftX = tabbedPaneUI.currentShiftXTmp;

            if (!leftToRight) {
                if (tabsButton.isVisible()) {
                    rightMargin -= tabsButton.getWidth();
                }
                JComponent newTabButton = tabbedPaneUI.scrollableTabSupport.newTabButton;
                if (newTabButton.isVisible() && newTabButton.getParent() == tabbedPaneUI.tabPane) {
                    rightMargin -= newTabButton.getWidth();
                }
                for (int i = 0; i < tabCount; i++) {
                    tabbedPaneUI.rects[i].x = rightMargin - tabbedPaneUI.rects[i].x - tabbedPaneUI.rects[i].width;
                }
            }
            if (tabbedPaneUI.scrollableTabSupport.newTabButton.isVisible()) {
                layoutNewTabButton(true, leftToRight, leftMargin, rightMargin, tabCount);
            }

        } else {
            int bottomMargin = size.height - (insets.bottom + tabAreaInsets.bottom + insets.top + tabAreaInsets.top);
            Point p = getMargins(tabPlacement);
            int topMargin = p.x;
            int returnAt = p.y;
            tabbedPaneUI.currentShiftYTmp = tabbedPaneUI.currentShiftY;
            shiftTabsY(0, topMargin, returnAt, tabCount, false);
            if (!(tabbedPaneUI.minVisible == tabbedPaneUI.maxVisible
                    && tabbedPaneUI.minVisible == tabbedPaneUI.tabPane.getSelectedIndex())) {
                selectedBounds.y += tabbedPaneUI.currentShiftYTmp;
                shiftBoundsToVisibleY(selectedBounds, topMargin, returnAt, tabCount);
            }
            restoreHiddenTabsY(topMargin, returnAt, tabCount);

            if (tabbedPaneUI.minVisible == 0 && tabbedPaneUI.maxVisible == tabCount - 1
                    && PropertyUtil.getBooleanProperty(tabbedPaneUI.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)) {
                adjustForCenterY(topMargin, returnAt, tabCount);
            }

            if (tabsButton.isVisible() && tabbedPaneUI.tabPane.getSelectedIndex() < tabbedPaneUI.maxVisible) {
                shiftTabsY(0, topMargin, bottomMargin, tabCount, false);
                if (tabbedPaneUI.minVisible > 0 || tabbedPaneUI.maxVisible < tabCount - 1) {
                    shiftTabsY(0, topMargin, returnAt, tabCount, false);
                }
            }
            adjustForDropY(topMargin, returnAt, tabCount);

            layoutMoreTabsButton(tabCount);
            commitShiftY(tabbedPaneUI.currentShiftYTmp, tabCount);
            tabbedPaneUI.currentShiftY = tabbedPaneUI.currentShiftYTmp;

            if (tabbedPaneUI.scrollableTabSupport.newTabButton.isVisible()) {
                layoutNewTabButton(false, leftToRight, topMargin, bottomMargin, tabCount);
            }
        }
        tabbedPaneUI.tabScroller.tabPanel.setPreferredSize(tabBounds.getSize());
    }

    @Override
    protected void centerTabs(final int tabPlacement, final int tabCount, final int returnAt) {
        // Do nothing.
    }

    protected void adjustForCenterX(final int leftMargin, final int returnAt, final int tabCount) {
        int shift = (returnAt - (tabbedPaneUI.rects[tabCount - 1].x + tabbedPaneUI.rects[tabCount - 1].width)) / 2;
        shiftTabsX(shift, 0, returnAt, tabCount - 1, true);
    }

    protected void adjustForCenterY(final int topMargin, final int returnAt, final int tabCount) {
        int shift = (returnAt - (tabbedPaneUI.rects[tabCount - 1].y + tabbedPaneUI.rects[tabCount - 1].height)) / 2;
        shiftTabsY(shift, 0, returnAt, tabCount - 1, true);
    }

    @Override
    protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
        return tabbedPaneUI.calculateMaxTabWidth(tabPlacement);
    }

    @Override
    protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
        return tabbedPaneUI.calculateMaxTabHeight(tabPlacement);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    protected Point getMargins(final int tabPlacement) {
        Dimension size = tabbedPaneUI.tabPane.getSize();
        Insets insets = tabbedPaneUI.tabPane.getInsets();
        Insets tabAreaInsets = tabbedPaneUI.getTabAreaInsets(tabPlacement);
        JComponent tabsButton = tabbedPaneUI.scrollableTabSupport.moreTabsButton;
        JComponent newTabsButton = tabbedPaneUI.scrollableTabSupport.newTabButton;
        if (tabbedPaneUI.isHorizontalTabPlacement()) {
            int leftMargin = 0;
            int returnAt = size.width - (insets.right + tabAreaInsets.right + insets.left + tabAreaInsets.left);
            if (tabsButton.isVisible()) {
                returnAt -= tabsButton.getPreferredSize().width;
            }
            if (newTabsButton.isVisible() && newTabsButton.getParent() == tabbedPaneUI.tabPane) {
                returnAt -= newTabsButton.getPreferredSize().width;
            }
            return new Point(leftMargin, returnAt);
        } else {
            int topMargin = 0;
            int returnAt = size.height - (insets.bottom + tabAreaInsets.bottom + insets.top + tabAreaInsets.top);
            if (tabsButton.isVisible()) {
                returnAt -= tabsButton.getPreferredSize().height;
            }
            if (newTabsButton.isVisible() && newTabsButton.getParent() == tabbedPaneUI.tabPane) {
                returnAt -= newTabsButton.getPreferredSize().height;
            }
            return new Point(topMargin, returnAt);
        }
    }

    protected void adjustForDropX(final int minX, final int maxX, final int tabCount) {
        if (tabbedPaneUI.dropSourceIndex >= 0 && tabbedPaneUI.dropSourceIndex < tabbedPaneUI.tabPane.getTabCount()) {
            // Hide the source tab.
            int shift = tabbedPaneUI.rects[tabbedPaneUI.dropSourceIndex].width;
            tabbedPaneUI.rects[tabbedPaneUI.dropSourceIndex].setSize(0, 0);
            commitShiftX(tabbedPaneUI.dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
        }
        if (tabbedPaneUI.sourceEqualsTarget && tabbedPaneUI.dropTargetIndex >= 0
                && tabbedPaneUI.dropTargetIndex < tabbedPaneUI.tabPane.getTabCount()) {
            commitShiftX(tabbedPaneUI.dropTargetIndex, tabCount - 1, tabbedPaneUI.dropRect.width, tabCount);
        }
        shiftTabsX(0, minX, maxX, tabCount, false);
    }

    protected void layoutNewTabButton(final boolean horizontal, final boolean leftToRight, final int minVal,
            final int maxVal, final int tabCount) {
        JComponent button = tabbedPaneUI.scrollableTabSupport.newTabButton;
        Dimension buttonBounds = button.getPreferredSize();
        if (horizontal) {
            if (leftToRight) {
                if (tabbedPaneUI.rects[tabCount - 1].x + tabbedPaneUI.rects[tabCount - 1].width
                        + buttonBounds.width > maxVal) {
                    if (button.getParent() != tabbedPaneUI.tabPane) tabbedPaneUI.tabPane.add(button);
                } else {
                    if (button.getParent() != tabbedPaneUI.scrollableTabSupport.tabPanel) {
                        tabbedPaneUI.scrollableTabSupport.tabPanel.add(button);
                    }
                }
            } else {
                int x = tabbedPaneUI.rects[tabCount - 1].x;
                if (x - buttonBounds.width < minVal) {
                    if (button.getParent() != tabbedPaneUI.tabPane) tabbedPaneUI.tabPane.add(button);
                } else {
                    if (button.getParent() != tabbedPaneUI.scrollableTabSupport.tabPanel) {
                        tabbedPaneUI.scrollableTabSupport.tabPanel.add(button);
                    }
                }
            }
        } else {
            if (tabbedPaneUI.rects[tabCount - 1].y + tabbedPaneUI.rects[tabCount - 1].height
                    + buttonBounds.height > maxVal) {
                if (button.getParent() != tabbedPaneUI.tabPane) tabbedPaneUI.tabPane.add(button);
            } else {
                if (button.getParent() != tabbedPaneUI.scrollableTabSupport.tabPanel) {
                    tabbedPaneUI.scrollableTabSupport.tabPanel.add(button);
                }
            }
        }
    }

    protected void commitShiftX(final int shift, final int tabCount) {
        commitShiftX(0, tabCount - 1, shift, tabCount);
    }

    protected void commitShiftX(final int low, final int high, final int shift, final int tabCount) {
        for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
            tabbedPaneUI.rects[i].x += shift;
        }
    }

    protected void commitShiftY(final int shift, final int tabCount) {
        commitShiftY(0, tabCount - 1, shift, tabCount);
    }

    protected void commitShiftY(final int low, final int high, final int shift, final int tabCount) {
        for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
            tabbedPaneUI.rects[i].y += shift;
        }
    }

    protected void shiftBoundsToVisibleX(final Rectangle selectedBounds, final int leftMargin, final int rightMargin,
            final int tabCount) {
        if (selectedBounds.x + selectedBounds.width > rightMargin) {
            // SelectedTab is not fully visible. Covered on right side.
            shiftTabsX(rightMargin - selectedBounds.x - selectedBounds.width, leftMargin, rightMargin, tabCount, true);
        } else if (selectedBounds.x < leftMargin) {
            // SelectedTab is not fully visible. Covered on left side.
            shiftTabsX(-selectedBounds.x + leftMargin, leftMargin, rightMargin, tabCount, true);
        }
    }

    protected void shiftBoundsToVisibleY(final Rectangle selectedBounds, final int topMargin, final int bottomMargin,
            final int tabCount) {
        if (selectedBounds.y + selectedBounds.height > bottomMargin) {
            // SelectedTab is not fully visible. Covered on right side.
            shiftTabsY(bottomMargin - selectedBounds.y - selectedBounds.height, topMargin, bottomMargin, tabCount,
                    true);
        } else if (selectedBounds.y < topMargin) {
            // SelectedTab is not fully visible. Covered on left side.
            shiftTabsY(-selectedBounds.y + topMargin, topMargin, bottomMargin, tabCount, true);
        }
    }

    protected void calculateRect(final int i, final Rectangle tabBounds, final FontMetrics metrics,
            final boolean verticalTabRuns, final int tabPlacement) {
        Rectangle rect = tabbedPaneUI.rects[i];
        if (!verticalTabRuns) {
            if (i > 0) {
                rect.x = tabbedPaneUI.rects[i - 1].x + tabbedPaneUI.rects[i - 1].width;
            } else {
                tabbedPaneUI.tabRuns[0] = 0;
                tabbedPaneUI.maxTabWidth = 0;
                rect.x = tabBounds.x;
            }
            rect.width = tabbedPaneUI.calculateTabWidth(tabPlacement, i, metrics);
            tabBounds.width = rect.x + rect.width;
            tabbedPaneUI.maxTabWidth = Math.max(tabbedPaneUI.maxTabWidth, rect.width);
            rect.height = tabBounds.height;
            rect.y = tabBounds.y + tabBounds.height - rect.height;
        } else {
            if (i > 0) {
                rect.y = tabbedPaneUI.rects[i - 1].y + tabbedPaneUI.rects[i - 1].height;
            } else {
                tabbedPaneUI.tabRuns[0] = 0;
                tabbedPaneUI.maxTabHeight = 0;
                rect.y = tabBounds.y;
            }
            rect.height = tabbedPaneUI.calculateTabHeight(tabPlacement, i, metrics.getHeight());
            tabBounds.height = rect.y + rect.height;
            tabbedPaneUI.maxTabHeight = Math.max(tabbedPaneUI.maxTabHeight, rect.height);
            rect.width = tabBounds.width;
            rect.x = tabBounds.x + tabBounds.width - rect.width;
        }
    }

    protected void restoreHiddenTabsX(final int minX, final int maxX, final int tabCount) {
        if (tabbedPaneUI.maxVisible < 0 || tabbedPaneUI.maxVisible >= tabCount) return;
        int space = Math.max(maxX - tabbedPaneUI.rects[tabbedPaneUI.maxVisible].x
                - tabbedPaneUI.rects[tabbedPaneUI.maxVisible].width - tabbedPaneUI.currentShiftXTmp, 0);
        int shift = Math.min(minX - tabbedPaneUI.rects[0].x - tabbedPaneUI.currentShiftXTmp, space);
        shiftTabsX(shift, minX, maxX, tabCount, true);
    }

    protected void restoreHiddenTabsY(final int minY, final int maxY, final int tabCount) {
        if (tabbedPaneUI.maxVisible < 0 || tabbedPaneUI.maxVisible >= tabCount) return;
        int space =
                Math.max(
                        maxY - tabbedPaneUI.rects[tabbedPaneUI.maxVisible].y
                                - tabbedPaneUI.rects[tabbedPaneUI.maxVisible].height - tabbedPaneUI.currentShiftYTmp,
                        0);
        int shift = Math.min(minY - tabbedPaneUI.rects[0].y - tabbedPaneUI.currentShiftYTmp, space);
        shiftTabsY(shift, minY, maxY, tabCount, true);
    }

    protected void adjustForDropY(final int minY, final int maxY, final int tabCount) {
        if (tabbedPaneUI.dropSourceIndex >= 0 && tabbedPaneUI.dropSourceIndex < tabbedPaneUI.tabPane.getTabCount()) {
            // Hide the source tab.
            int shift = tabbedPaneUI.rects[tabbedPaneUI.dropSourceIndex].height;
            tabbedPaneUI.rects[tabbedPaneUI.dropSourceIndex].setSize(0, 0);
            commitShiftY(tabbedPaneUI.dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
        }
        if (tabbedPaneUI.sourceEqualsTarget && tabbedPaneUI.dropTargetIndex >= 0
                && tabbedPaneUI.dropTargetIndex < tabbedPaneUI.tabPane.getTabCount()) {
            commitShiftY(tabbedPaneUI.dropTargetIndex, tabCount - 1, tabbedPaneUI.dropRect.height, tabCount);
        }
        shiftTabsY(0, minY, maxY, tabCount, false);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public void updateVisibleRange(final int tapPlacement) {
        Point p = getMargins(tapPlacement);
        if (tabbedPaneUI.isHorizontalTabPlacement()) {
            shiftTabsX(0, p.x, p.y, tabbedPaneUI.tabPane.getTabCount(), false);
        } else {
            shiftTabsY(0, p.x, p.y, tabbedPaneUI.tabPane.getTabCount(), false);
        }
    }

    protected void layoutMoreTabsButton(final int tabCount) {
        final JComponent button = tabbedPaneUI.scrollableTabSupport.moreTabsButton;
        if (tabbedPaneUI.minVisible > 0 || tabbedPaneUI.maxVisible < tabCount - 1) {
            if (tabbedPaneUI.scrollableTabSupport.moreTabsButton.isVisible()) {
                if (tabbedPaneUI.minVisible != tabbedPaneUI.minVisibleOld
                        || tabbedPaneUI.maxVisible != tabbedPaneUI.maxVisibleOld
                        || tabbedPaneUI.oldTabCount != tabCount) {
                    tabbedPaneUI.scrollableTabSupport.showMoreTabsButton();
                }
            } else {
                tabbedPaneUI.scrollableTabSupport.showMoreTabsButton();
            }
            // Update old values.
            tabbedPaneUI.minVisibleOld = tabbedPaneUI.minVisible;
            tabbedPaneUI.maxVisibleOld = tabbedPaneUI.maxVisible;
            tabbedPaneUI.oldTabCount = tabCount;
        } else if (button.isVisible()) {
            tabbedPaneUI.scrollableTabSupport.hideMoreTabsButton();
        }
    }

    protected void shiftTabsX(final int shift, final int minX, final int returnAt, final int tabCount,
            final boolean updateShift) {
        shiftTabs(shift, minX, returnAt, tabCount, updateShift, true);
    }

    protected void shiftTabsY(final int shift, final int minY, final int returnAt, final int tabCount,
            final boolean updateShift) {
        shiftTabs(shift, minY, returnAt, tabCount, updateShift, false);
    }

    protected void shiftTabs(final int shift, final int minVal, final int returnAt, final int tabCount,
            final boolean updateShift, final boolean isX) {
        int min = -1;
        int max = -1;
        int minStart = tabbedPaneUI.minVisible < 0 || tabbedPaneUI.minVisible >= tabCount ? 0 : tabbedPaneUI.minVisible;
        int maxStart = tabbedPaneUI.maxVisible < 0 || tabbedPaneUI.maxVisible >= tabCount ? tabCount - 1
                : tabbedPaneUI.maxVisible;
        int currShift = isX ? tabbedPaneUI.currentShiftXTmp + shift : tabbedPaneUI.currentShiftYTmp + shift;
        Function<Integer, Boolean> isVisible = isX ? i -> isVisibleX(i, currShift, minVal, returnAt)
                : i -> isVisibleY(i, currShift, minVal, returnAt);
        if (isVisible.apply(minStart)) {
            // Descent to find minimum.
            min = minStart;
            for (int i = minStart - 1; i >= 0; i--) {
                if (isVisible.apply(i)) {
                    min = i;
                } else {
                    break;
                }
            }
        } else {
            // Ascent to find minimum.
            for (int i = minStart + 1; i < tabCount; i++) {
                if (isVisible.apply(i)) {
                    min = i;
                    break;
                }
            }
        }
        if (min == -1) {
            min = tabCount;
        }
        if (isVisible.apply(maxStart)) {
            // Ascent to find maximum.
            max = maxStart;
            for (int i = maxStart + 1; i < tabCount; i++) {
                if (isVisible.apply(i)) {
                    max = i;
                } else {
                    break;
                }
            }
        } else {
            // Descent to find maximum.
            for (int i = maxStart - 1; i >= 0; i--) {
                if (isVisible.apply(i)) {
                    max = i;
                    break;
                }
            }
        }
        tabbedPaneUI.minVisible = min;
        tabbedPaneUI.maxVisible = max;

        if (updateShift) {
            if (isX) {
                tabbedPaneUI.currentShiftXTmp += shift;
            } else {
                tabbedPaneUI.currentShiftYTmp += shift;
            }
        }
    }

    protected boolean isVisibleX(final int i, final int shift, final int minX, final int maxX) {
        int begin = tabbedPaneUI.rects[i].x + shift;
        int end = begin + tabbedPaneUI.rects[i].width;
        return !(begin >= maxX || end < minX);
    }

    protected boolean isVisibleY(final int i, final int shift, final int minX, final int maxX) {
        int begin = tabbedPaneUI.rects[i].y + shift;
        int end = begin + tabbedPaneUI.rects[i].height;
        return !(begin >= maxX || end < minX);
    }
}
