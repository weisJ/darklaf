/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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

    private final DarkTabbedPaneUI ui;

    public DarkTabbedPaneScrollLayout(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    public void layoutContainer(final Container parent) {
        int tabPlacement = ui.tabPane.getTabPlacement();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);
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
            ui.scrollableTabSupport.hideMoreTabsButton();
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
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
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
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
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
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
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
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    tx = insets.left + tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + th + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - th - contentInsets.top - contentInsets.bottom;
                    th -= tabAreaInsets.top + tabAreaInsets.bottom;
                    break;
            }
            JComponent moreTabs = ui.scrollableTabSupport.moreTabsButton;
            JComponent newTab = ui.scrollableTabSupport.newTabButton;

            for (int i = 0; i < numChildren; i++) {
                Component child = ui.tabPane.getComponent(i);

                if (ui.tabScroller != null && child == ui.tabScroller.viewport) {
                    int vw = tw;
                    int vh = th;
                    Dimension butSize = moreTabs.isVisible() ? moreTabs.getPreferredSize() : new Dimension(0, 0);
                    boolean showNewTabButton = newTab.isVisible() && newTab.getParent() == ui.tabPane;
                    Dimension butSize2 = showNewTabButton ? newTab.getPreferredSize() : new Dimension(0, 0);
                    boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();
                    if (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT) {
                        vh = th - butSize.height - butSize2.height;
                        moreTabs.setBounds(tx, ty + vh + butSize2.height, ui.maxTabWidth, butSize.height);
                        if (showNewTabButton) {
                            newTab.setBounds(tx, ty + vh, ui.maxTabWidth, butSize2.height);
                        }
                    } else {
                        if (leftToRight) {
                            vw = tw - butSize.width - butSize2.width;
                            moreTabs.setBounds(tx + vw + butSize2.width, ty, butSize.width, ui.maxTabHeight);
                            if (showNewTabButton) {
                                newTab.setBounds(tx + vw, ty, butSize2.width, ui.maxTabHeight);
                            }
                        } else {
                            vw = tw - butSize.width - butSize2.width;
                            moreTabs.setBounds(tx, ty, butSize.width, ui.maxTabHeight);
                            if (showNewTabButton) {
                                newTab.setBounds(tx + butSize.width, ty, butSize2.width, ui.maxTabHeight);
                            }
                            tx += butSize.width + butSize2.width;
                        }
                    }
                    child.setBounds(tx, ty, vw, vh);
                } else {
                    int tabHeight = ui.maxTabHeight + tabAreaInsets.top + tabAreaInsets.bottom;
                    int tabWidth = ui.maxTabWidth + tabAreaInsets.left + tabAreaInsets.right;
                    int compHeight = ch;
                    int compY = cy;
                    if (ui.northComp != null) {
                        int nh = ui.northComp.getPreferredSize().height;
                        compY -= nh;
                        compHeight += nh;
                    }
                    if (ui.southComp != null) {
                        compHeight += ui.southComp.getPreferredSize().height;
                    }
                    if (child == ui.leadingComp && ui.leadingComp != null) {
                        ui.layoutLeadingComponent(child, tabWidth, tabHeight, insets, tx, ty, tabPlacement);
                    } else if (child == ui.trailingComp && ui.trailingComp != null) {
                        ui.layoutTrailingComponent(child, tabWidth, tabHeight, insets, tx, ty, tw, th, tabPlacement);
                    } else if (child == ui.northComp && ui.northComp != null) {
                        ui.northComp.setBounds(
                            cx, cy - ui.northComp.getPreferredSize().height, cw, ui.northComp.getPreferredSize().height
                        );
                    } else if (child == ui.southComp && ui.southComp != null) {
                        ui.southComp.setBounds(cx, cy + ch, cw, ui.southComp.getPreferredSize().height);
                    } else if (child == ui.eastComp && ui.eastComp != null) {
                        ui.eastComp.setBounds(cx + cw, compY, ui.eastComp.getPreferredSize().width, compHeight);
                    } else if (child == ui.westComp && ui.westComp != null) {
                        ui.westComp.setBounds(
                            cx - ui.westComp.getPreferredSize().width, compY, ui.westComp.getPreferredSize().width,
                            compHeight
                        );
                    } else if (child != moreTabs && child != newTab) {
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
    }

    @Override
    protected void layoutTabComponents() {
        ui.layoutTabComponents();
    }

    @Override
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
        if (ui.tabPane.getTabCount() > 0) {
            if (!ui.isHorizontalTabPlacement()) {
                int tabHeight =
                    ui.calculateTabHeight(tabPlacement, ui.tabPane.getSelectedIndex(), ui.getFontMetrics().getHeight());
                if (ui.scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabHeight += ui.scrollableTabSupport.moreTabsButton.getPreferredSize().height;
                }
                height = Math.max(height, tabHeight);
                tabExtent = preferredTabAreaWidth(tabPlacement, height - tabAreaInsets.top - tabAreaInsets.bottom);
                width += tabExtent;
            } else {
                int tabWidth = ui.calculateTabWidth(tabPlacement, ui.tabPane.getSelectedIndex(), ui.getFontMetrics());
                if (ui.scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabWidth += ui.scrollableTabSupport.moreTabsButton.getPreferredSize().width;
                }
                width = Math.max(width, tabWidth);
                tabExtent = preferredTabAreaHeight(tabPlacement, width - tabAreaInsets.left - tabAreaInsets.right);
                height += tabExtent;
            }
        }
        return new Dimension(
            width + insets.left + insets.right + contentInsets.left + contentInsets.right,
            height + insets.bottom + insets.top + contentInsets.top + contentInsets.bottom
        );

    }

    @SuppressWarnings("SuspiciousNameCombination")
    @Override
    protected void calculateTabRects(final int tabPlacement, final int tabCount) {
        FontMetrics metrics = ui.getFontMetrics();
        Dimension size = ui.tabPane.getSize();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);
        boolean verticalTabRuns = !ui.isHorizontalTabPlacement();
        boolean leftToRight = ui.tabPane.getComponentOrientation().isLeftToRight();

        if (!ui.isHorizontalTabPlacement()) {
            ui.maxTabWidth = ui.calculateMaxTabWidth(tabPlacement);
        } else {
            ui.maxTabHeight = ui.calculateMaxTabHeight(tabPlacement);
        }

        ui.runCount = 0;
        ui.selectedRun = -1;

        if (tabCount == 0) return;

        ui.selectedRun = 0;
        ui.runCount = 1;

        Rectangle tabBounds = new Rectangle(0, 0, 0, 0);
        for (int i = 0; i < tabCount; i++) {
            calculateRect(i, tabBounds, metrics, verticalTabRuns, tabPlacement);
        }

        JComponent tabsButton = ui.scrollableTabSupport.moreTabsButton;
        Rectangle selectedBounds = ui.tabPane.getSelectedIndex() > 0
            ? new Rectangle(ui.rects[ui.tabPane.getSelectedIndex()])
            : new Rectangle(0, 0, 0, 0);
        if (!verticalTabRuns) {
            int rightMargin = size.width - (insets.right + tabAreaInsets.right + insets.left + tabAreaInsets.left);
            Point p = getMargins(tabPlacement);
            int leftMargin = p.x;
            int returnAt = p.y;

            ui.currentShiftXTmp = ui.currentShiftX;
            shiftTabsX(0, leftMargin, returnAt, tabCount, false);
            if (!(ui.minVisible == ui.maxVisible && ui.minVisible == ui.tabPane.getSelectedIndex())) {
                selectedBounds.x += ui.currentShiftXTmp;
                shiftBoundsToVisibleX(selectedBounds, leftMargin, returnAt, tabCount);
            }
            restoreHiddenTabsX(leftMargin, returnAt, tabCount);

            if (
                ui.minVisible == 0 && ui.maxVisible == tabCount - 1
                    && PropertyUtil.getBooleanProperty(ui.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)
            ) {
                adjustForCenterX(leftMargin, returnAt, tabCount);
            }

            if (tabsButton.isVisible() && ui.tabPane.getSelectedIndex() < ui.maxVisible) {
                // Shift again. Hiding the the tab button might reveal the last tab.
                // Only do this if the last visible tab is not currently selected.
                // Otherwise the selected tab forces the whole tab area the jump by the width of the tab button.
                int margin = returnAt + tabsButton.getPreferredSize().width;
                shiftTabsX(0, leftMargin, margin, tabCount, false);
                if (ui.minVisible > 0 || ui.maxVisible < tabCount - 1) {
                    // Tab button is still visible but may hide a further tab. restore visible bounds.
                    shiftTabsX(0, leftMargin, returnAt, tabCount, false);
                }
            }
            adjustForDropX(leftMargin, returnAt, tabCount);

            layoutMoreTabsButton(tabCount);

            commitShiftX(ui.currentShiftXTmp, tabCount);
            ui.currentShiftX = ui.currentShiftXTmp;

            if (!leftToRight) {
                if (tabsButton.isVisible()) {
                    rightMargin -= tabsButton.getWidth();
                }
                JComponent newTabButton = ui.scrollableTabSupport.newTabButton;
                if (newTabButton.isVisible() && newTabButton.getParent() == ui.tabPane) {
                    rightMargin -= newTabButton.getWidth();
                }
                for (int i = 0; i < tabCount; i++) {
                    ui.rects[i].x = rightMargin - ui.rects[i].x - ui.rects[i].width;
                }
            }
            if (ui.scrollableTabSupport.newTabButton.isVisible()) {
                layoutNewTabButton(true, leftToRight, leftMargin, returnAt, tabCount);
            }

        } else {
            int bottomMargin = size.height - (insets.bottom + tabAreaInsets.bottom + insets.top + tabAreaInsets.top);
            Point p = getMargins(tabPlacement);
            int topMargin = p.x;
            int returnAt = p.y;
            ui.currentShiftYTmp = ui.currentShiftY;
            shiftTabsY(0, topMargin, returnAt, tabCount, false);
            if (!(ui.minVisible == ui.maxVisible && ui.minVisible == ui.tabPane.getSelectedIndex())) {
                selectedBounds.y += ui.currentShiftYTmp;
                shiftBoundsToVisibleY(selectedBounds, topMargin, returnAt, tabCount);
            }
            restoreHiddenTabsY(topMargin, returnAt, tabCount);

            if (
                ui.minVisible == 0 && ui.maxVisible == tabCount - 1
                    && PropertyUtil.getBooleanProperty(ui.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)
            ) {
                adjustForCenterY(topMargin, returnAt, tabCount);
            }

            if (tabsButton.isVisible() && ui.tabPane.getSelectedIndex() < ui.maxVisible) {
                shiftTabsY(0, topMargin, bottomMargin, tabCount, false);
                if (ui.minVisible > 0 || ui.maxVisible < tabCount - 1) {
                    shiftTabsY(0, topMargin, returnAt, tabCount, false);
                }
            }
            adjustForDropY(topMargin, returnAt, tabCount);

            layoutMoreTabsButton(tabCount);
            commitShiftY(ui.currentShiftYTmp, tabCount);
            ui.currentShiftY = ui.currentShiftYTmp;

            if (ui.scrollableTabSupport.newTabButton.isVisible()) {
                layoutNewTabButton(false, leftToRight, topMargin, returnAt, tabCount);
            }
        }
        ui.tabScroller.tabPanel.setPreferredSize(tabBounds.getSize());
    }

    @Override
    protected void centerTabs(final int tabPlacement, final int tabCount, final int returnAt) {
        // Do nothing.
    }

    protected void adjustForCenterX(final int leftMargin, final int returnAt, final int tabCount) {
        int shift = (returnAt - (ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width)) / 2;
        shiftTabsX(shift, 0, returnAt, tabCount - 1, true);
    }

    protected void adjustForCenterY(final int topMargin, final int returnAt, final int tabCount) {
        int shift = (returnAt - (ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height)) / 2;
        shiftTabsY(shift, 0, returnAt, tabCount - 1, true);
    }

    @Override
    protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
        return ui.calculateMaxTabWidth(tabPlacement);
    }

    @Override
    protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
        return ui.calculateMaxTabHeight(tabPlacement);
    }

    @SuppressWarnings("SuspiciousNameCombination")

    protected Point getMargins(final int tabPlacement) {
        Dimension size = ui.tabPane.getSize();
        Insets insets = ui.tabPane.getInsets();
        Insets tabAreaInsets = ui.getTabAreaInsets(tabPlacement);
        JComponent tabsButton = ui.scrollableTabSupport.moreTabsButton;
        JComponent newTabsButton = ui.scrollableTabSupport.newTabButton;
        if (ui.isHorizontalTabPlacement()) {
            int leftMargin = 0;
            int returnAt = size.width - (insets.right + tabAreaInsets.right + insets.left + tabAreaInsets.left);
            if (tabsButton.isVisible()) {
                returnAt -= tabsButton.getPreferredSize().width;
            }
            if (newTabsButton.isVisible() && newTabsButton.getParent() == ui.tabPane) {
                returnAt -= newTabsButton.getPreferredSize().width;
            }
            return new Point(leftMargin, returnAt);
        } else {
            int topMargin = 0;
            int returnAt = size.height - (insets.bottom + tabAreaInsets.bottom + insets.top + tabAreaInsets.top);
            if (tabsButton.isVisible()) {
                returnAt -= tabsButton.getPreferredSize().height;
            }
            if (newTabsButton.isVisible() && newTabsButton.getParent() == ui.tabPane) {
                returnAt -= newTabsButton.getPreferredSize().height;
            }
            return new Point(topMargin, returnAt);
        }
    }

    protected void adjustForDropX(final int minX, final int maxX, final int tabCount) {
        if (ui.dropSourceIndex >= 0 && ui.dropSourceIndex < ui.tabPane.getTabCount()) {
            // Hide the source tab.
            int shift = ui.rects[ui.dropSourceIndex].width;
            ui.rects[ui.dropSourceIndex].setSize(0, 0);
            commitShiftX(ui.dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
        }
        if (ui.sourceEqualsTarget && ui.dropTargetIndex >= 0 && ui.dropTargetIndex < ui.tabPane.getTabCount()) {
            commitShiftX(ui.dropTargetIndex, tabCount - 1, ui.dropRect.width, tabCount);
        }
        shiftTabsX(0, minX, maxX, tabCount, false);
    }

    protected void layoutNewTabButton(
            final boolean horizontal, final boolean leftToRight, final int minVal, final int maxVal, final int tabCount
    ) {
        JComponent button = ui.scrollableTabSupport.newTabButton;
        Dimension buttonBounds = button.getPreferredSize();
        if (horizontal) {
            if (leftToRight) {
                if (ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width + buttonBounds.width > maxVal) {
                    if (button.getParent() != ui.tabPane) ui.tabPane.add(button);
                } else {
                    if (button.getParent() != ui.scrollableTabSupport.tabPanel) {
                        ui.scrollableTabSupport.tabPanel.add(button);
                    }
                }
            } else {
                int x = ui.rects[tabCount - 1].x;
                if (x - buttonBounds.width < minVal) {
                    if (button.getParent() != ui.tabPane) ui.tabPane.add(button);
                } else {
                    if (button.getParent() != ui.scrollableTabSupport.tabPanel) {
                        ui.scrollableTabSupport.tabPanel.add(button);
                    }
                }
            }
        } else {
            if (ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height + buttonBounds.height > maxVal) {
                if (button.getParent() != ui.tabPane) ui.tabPane.add(button);
            } else {
                if (button.getParent() != ui.scrollableTabSupport.tabPanel) {
                    ui.scrollableTabSupport.tabPanel.add(button);
                }
            }
        }
    }

    protected void commitShiftX(final int shift, final int tabCount) {
        commitShiftX(0, tabCount - 1, shift, tabCount);
    }

    protected void commitShiftX(final int low, final int high, final int shift, final int tabCount) {
        for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
            ui.rects[i].x += shift;
        }
    }

    protected void commitShiftY(final int shift, final int tabCount) {
        commitShiftY(0, tabCount - 1, shift, tabCount);
    }

    protected void commitShiftY(final int low, final int high, final int shift, final int tabCount) {
        for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
            ui.rects[i].y += shift;
        }
    }

    protected void shiftBoundsToVisibleX(
            final Rectangle selectedBounds, final int leftMargin, final int rightMargin, final int tabCount
    ) {
        if (selectedBounds.x + selectedBounds.width > rightMargin) {
            // SelectedTab is not fully visible. Covered on right side.
            shiftTabsX(rightMargin - selectedBounds.x - selectedBounds.width, leftMargin, rightMargin, tabCount, true);
        } else if (selectedBounds.x < leftMargin) {
            // SelectedTab is not fully visible. Covered on left side.
            shiftTabsX(-selectedBounds.x + leftMargin, leftMargin, rightMargin, tabCount, true);
        }
    }

    protected void shiftBoundsToVisibleY(
            final Rectangle selectedBounds, final int topMargin, final int bottomMargin, final int tabCount
    ) {
        if (selectedBounds.y + selectedBounds.height > bottomMargin) {
            // SelectedTab is not fully visible. Covered on right side.
            shiftTabsY(
                bottomMargin - selectedBounds.y - selectedBounds.height, topMargin, bottomMargin, tabCount, true
            );
        } else if (selectedBounds.y < topMargin) {
            // SelectedTab is not fully visible. Covered on left side.
            shiftTabsY(-selectedBounds.y + topMargin, topMargin, bottomMargin, tabCount, true);
        }
    }

    protected void calculateRect(
            final int i, final Rectangle tabBounds, final FontMetrics metrics, final boolean verticalTabRuns,
            final int tabPlacement
    ) {
        Rectangle rect = ui.rects[i];
        if (!verticalTabRuns) {
            if (i > 0) {
                rect.x = ui.rects[i - 1].x + ui.rects[i - 1].width;
            } else {
                ui.tabRuns[0] = 0;
                ui.maxTabWidth = 0;
                tabBounds.height = ui.maxTabHeight;
                rect.x = tabBounds.x;
            }
            rect.width = ui.calculateTabWidth(tabPlacement, i, metrics);
            tabBounds.width = rect.x + rect.width;
            ui.maxTabWidth = Math.max(ui.maxTabWidth, rect.width);
            rect.y = tabBounds.y;
            rect.height = ui.maxTabHeight;
        } else {
            if (i > 0) {
                rect.y = ui.rects[i - 1].y + ui.rects[i - 1].height;
            } else {
                ui.tabRuns[0] = 0;
                ui.maxTabHeight = 0;
                tabBounds.width = ui.maxTabWidth;
                rect.y = tabBounds.y;
            }
            rect.height = ui.calculateTabHeight(tabPlacement, i, metrics.getHeight());
            tabBounds.height = rect.y + rect.height;
            ui.maxTabHeight = Math.max(ui.maxTabHeight, rect.height);
            rect.x = tabBounds.x;
            rect.width = ui.maxTabWidth;
        }
    }

    protected void restoreHiddenTabsX(final int minX, final int maxX, final int tabCount) {
        if (ui.maxVisible < 0 || ui.maxVisible >= tabCount) return;
        int space = Math.max(maxX - ui.rects[ui.maxVisible].x - ui.rects[ui.maxVisible].width - ui.currentShiftXTmp, 0);
        int shift = Math.min(minX - ui.rects[0].x - ui.currentShiftXTmp, space);
        shiftTabsX(shift, minX, maxX, tabCount, true);
    }

    protected void restoreHiddenTabsY(final int minY, final int maxY, final int tabCount) {
        if (ui.maxVisible < 0 || ui.maxVisible >= tabCount) return;
        int space =
            Math.max(maxY - ui.rects[ui.maxVisible].y - ui.rects[ui.maxVisible].height - ui.currentShiftYTmp, 0);
        int shift = Math.min(minY - ui.rects[0].y - ui.currentShiftYTmp, space);
        shiftTabsY(shift, minY, maxY, tabCount, true);
    }

    protected void adjustForDropY(final int minY, final int maxY, final int tabCount) {
        if (ui.dropSourceIndex >= 0 && ui.dropSourceIndex < ui.tabPane.getTabCount()) {
            // Hide the source tab.
            int shift = ui.rects[ui.dropSourceIndex].height;
            ui.rects[ui.dropSourceIndex].setSize(0, 0);
            commitShiftY(ui.dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
        }
        if (ui.sourceEqualsTarget && ui.dropTargetIndex >= 0 && ui.dropTargetIndex < ui.tabPane.getTabCount()) {
            commitShiftY(ui.dropTargetIndex, tabCount - 1, ui.dropRect.height, tabCount);
        }
        shiftTabsY(0, minY, maxY, tabCount, false);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public void updateVisibleRange(final int tapPlacement) {
        Point p = getMargins(tapPlacement);
        if (ui.isHorizontalTabPlacement()) {
            shiftTabsX(0, p.x, p.y, ui.tabPane.getTabCount(), false);
        } else {
            shiftTabsY(0, p.x, p.y, ui.tabPane.getTabCount(), false);
        }
    }

    protected void layoutMoreTabsButton(final int tabCount) {
        final JComponent button = ui.scrollableTabSupport.moreTabsButton;
        if (ui.minVisible > 0 || ui.maxVisible < tabCount - 1) {
            if (ui.scrollableTabSupport.moreTabsButton.isVisible()) {
                if (ui.minVisible != ui.minVisibleOld || ui.maxVisible != ui.maxVisibleOld) {
                    ui.scrollableTabSupport.showMoreTabsButton();
                }
            } else {
                ui.scrollableTabSupport.showMoreTabsButton();
            }
            // Update old values.
            ui.minVisibleOld = ui.minVisible;
            ui.maxVisibleOld = ui.maxVisible;
        } else if (button.isVisible()) {
            ui.scrollableTabSupport.hideMoreTabsButton();
        }
    }

    protected void shiftTabsX(
            final int shift, final int minX, final int returnAt, final int tabCount, final boolean updateShift
    ) {
        shiftTabs(shift, minX, returnAt, tabCount, updateShift, true);
    }

    protected void shiftTabsY(
            final int shift, final int minY, final int returnAt, final int tabCount, final boolean updateShift
    ) {
        shiftTabs(shift, minY, returnAt, tabCount, updateShift, false);
    }

    protected void shiftTabs(
            final int shift, final int minVal, final int returnAt, final int tabCount, final boolean updateShift,
            final boolean isX
    ) {
        int min = -1;
        int max = -1;
        int minStart = ui.minVisible < 0 || ui.minVisible >= tabCount ? 0 : ui.minVisible;
        int maxStart = ui.maxVisible < 0 || ui.maxVisible >= tabCount ? tabCount - 1 : ui.maxVisible;
        int currShift = isX ? ui.currentShiftXTmp + shift : ui.currentShiftYTmp + shift;
        Function<Integer, Boolean> isVisible =
            isX ? (i -> isVisibleX(i, currShift, minVal, returnAt)) : (i -> isVisibleY(i, currShift, minVal, returnAt));
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
        ui.minVisible = min;
        ui.maxVisible = max;

        if (updateShift) {
            if (isX) {
                ui.currentShiftXTmp += shift;
            } else {
                ui.currentShiftYTmp += shift;
            }
        }
    }

    protected boolean isVisibleX(final int i, final int shift, final int minX, final int maxX) {
        int begin = ui.rects[i].x + shift;
        int end = begin + ui.rects[i].width;
        return !(begin >= maxX || end < minX);
    }

    protected boolean isVisibleY(final int i, final int shift, final int minX, final int maxX) {
        int begin = ui.rects[i].y + shift;
        int end = begin + ui.rects[i].height;
        return !(begin >= maxX || end < minX);
    }
}
