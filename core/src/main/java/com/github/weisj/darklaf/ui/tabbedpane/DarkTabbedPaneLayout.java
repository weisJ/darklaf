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

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTabbedPaneLayout extends TabbedPaneLayout {

    private final DarkTabbedPaneUI ui;

    public DarkTabbedPaneLayout(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    @Override
    protected void centerTabs(final int tabPlacement, final int tabCount, final int returnAt) {
        if (ui.runCount == 1 && PropertyUtil.getBooleanProperty(ui.tabPane, DarkTabbedPaneUI.KEY_CENTER_TABS)) {
            if (ui.isHorizontalTabPlacement()) {
                int shift = (returnAt - (ui.rects[tabCount - 1].x + ui.rects[tabCount - 1].width)) / 2;
                for (int i = 0; i < tabCount; i++) {
                    ui.rects[i].x += shift;
                }
            } else {
                int shift = (returnAt - (ui.rects[tabCount - 1].y + ui.rects[tabCount - 1].height)) / 2;
                for (int i = 0; i < tabCount; i++) {
                    ui.rects[i].y += shift;
                }
            }
        }
    }

    /*
     * Non scroll-layout
     */
    @Override
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
        int cx, cy, cw, ch;
        int tx, ty, tw, th;
        Insets contentInsets = ui.getContentBorderInsets(tabPlacement);

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

        Rectangle bounds = ui.tabPane.getBounds();
        int numChildren = ui.tabPane.getComponentCount();

        if (numChildren > 0) {
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
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
                    tw = ui.calculateTabAreaWidth(tabPlacement, ui.runCount, ui.maxTabWidth);
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
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
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
                    th = ui.calculateTabAreaHeight(tabPlacement, ui.runCount, ui.maxTabHeight);
                    cx = insets.left + contentInsets.left;
                    cy = insets.top + th + contentInsets.top + tabAreaInsets.top + tabAreaInsets.bottom;
                    cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                    ch = bounds.height - th - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom
                            - tabAreaInsets.top - tabAreaInsets.bottom;
                    break;
            }

            ui.tabAreaBounds.setRect(tx, ty, tw, th);
            for (int i = 0; i < numChildren; i++) {
                Component child = ui.tabPane.getComponent(i);
                if (child == ui.tabContainer) {
                    child.setBounds(tx, ty, tw, th);
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
                        ui.northComp.setBounds(cx, cy - ui.northComp.getPreferredSize().height, cw,
                                ui.northComp.getPreferredSize().height);
                    } else if (child == ui.southComp && ui.southComp != null) {
                        ui.southComp.setBounds(cx, cy + ch, cw, ui.southComp.getPreferredSize().height);
                    } else if (child == ui.eastComp && ui.eastComp != null) {
                        ui.eastComp.setBounds(cx + cw, compY, ui.eastComp.getPreferredSize().width, compHeight);
                    } else if (child == ui.westComp && ui.westComp != null) {
                        ui.westComp.setBounds(cx - ui.westComp.getPreferredSize().width, compY,
                                ui.westComp.getPreferredSize().width, compHeight);
                    } else {
                        child.setBounds(cx, cy, cw, ch);
                    }
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

    @Override
    protected void layoutTabComponents() {
        ui.layoutTabComponents();
    }
}
