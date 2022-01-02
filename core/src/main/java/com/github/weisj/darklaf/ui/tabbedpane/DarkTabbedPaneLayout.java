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

import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTabbedPaneLayout extends TabbedPaneLayout {

    private final DarkTabbedPaneUI tabbedPaneUI;

    public DarkTabbedPaneLayout(final DarkTabbedPaneUI tabbedPaneUI) {
        super(tabbedPaneUI);
        this.tabbedPaneUI = tabbedPaneUI;
    }

    @Override
    protected void centerTabs(final int tabPlacement, final int tabCount, final int returnAt) {
        if (tabbedPaneUI.runCount == 1
                && PropertyUtil.getBooleanProperty(tabbedPaneUI.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)) {
            if (tabbedPaneUI.isHorizontalTabPlacement()) {
                int shift =
                        (returnAt - (tabbedPaneUI.rects[tabCount - 1].x + tabbedPaneUI.rects[tabCount - 1].width)) / 2;
                for (int i = 0; i < tabCount; i++) {
                    tabbedPaneUI.rects[i].x += shift;
                }
            } else {
                int shift =
                        (returnAt - (tabbedPaneUI.rects[tabCount - 1].y + tabbedPaneUI.rects[tabCount - 1].height)) / 2;
                for (int i = 0; i < tabCount; i++) {
                    tabbedPaneUI.rects[i].y += shift;
                }
            }
        }
    }

    /*
     * Non scroll-layout
     */
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
        int cx, cy, cw, ch;
        int tx, ty, tw, th;
        Insets contentInsets = tabbedPaneUI.getContentBorderInsets(tabPlacement);

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

        Rectangle bounds = tabbedPaneUI.tabPane.getBounds();
        int numChildren = tabbedPaneUI.tabPane.getComponentCount();

        if (numChildren > 0) {
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    tw = tabbedPaneUI.calculateTabAreaWidth(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabWidth);
                    tx = insets.left + tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    th = bounds.height - insets.top - tabAreaInsets.top - insets.bottom - tabAreaInsets.bottom;
                    cx = insets.left + tw + contentInsets.left + tabAreaInsets.left + tabAreaInsets.right;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right - tw
                            - tabAreaInsets.left - tabAreaInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                    break;
                case SwingConstants.RIGHT:
                    tw = tabbedPaneUI.calculateTabAreaWidth(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabWidth);
                    tx = bounds.width - insets.left - tw - tabAreaInsets.right - tabAreaInsets.left;
                    ty = insets.top + tabAreaInsets.top;
                    th = bounds.height - insets.top - tabAreaInsets.top - insets.bottom - tabAreaInsets.bottom;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right - tw
                            - tabAreaInsets.left - tabAreaInsets.right;
                    ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                    break;
                case SwingConstants.BOTTOM:
                    th = tabbedPaneUI.calculateTabAreaHeight(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabHeight);
                    ty = bounds.height - insets.bottom - th;
                    tx = insets.left + tabAreaInsets.left;
                    tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + contentInsets.top;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - th - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                    break;
                default:
                    ty = insets.top + tabAreaInsets.top;
                    tx = insets.left + tabAreaInsets.left;
                    tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                    th = tabbedPaneUI.calculateTabAreaHeight(tabPlacement, tabbedPaneUI.runCount,
                            tabbedPaneUI.maxTabHeight);
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + th + contentInsets.top + tabAreaInsets.top + tabAreaInsets.bottom;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - th - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom
                            - tabAreaInsets.top - tabAreaInsets.bottom;
                    break;
            }

            tabbedPaneUI.tabAreaBounds.setRect(tx, ty, tw, th);
            for (int i = 0; i < numChildren; i++) {
                Component child = tabbedPaneUI.tabPane.getComponent(i);
                if (child == tabbedPaneUI.tabContainer) {
                    child.setBounds(tx, ty, tw, th);
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
                    } else {
                        child.setBounds(cx, cy, cw, ch);
                    }
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

    @Override
    protected void layoutTabComponents() {
        tabbedPaneUI.layoutTabComponents();
    }
}
