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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

class ScrollableTabSupport implements ActionListener, ChangeListener {
    private DarkTabbedPaneUIBridge ui;
    public final CroppedEdge croppedEdge;
    protected final Point tabViewPosition = new Point(0, 0);
    public ScrollableTabViewport viewport;
    public ScrollableTabPanel tabPanel;
    public JButton scrollForwardButton;
    public JButton scrollBackwardButton;
    public int leadingTabIndex;

    ScrollableTabSupport(final DarkTabbedPaneUIBridge ui, final int tabPlacement) {
        this.ui = ui;
        viewport = new ScrollableTabViewport(ui);
        tabPanel = new ScrollableTabPanel(ui);
        viewport.setView(tabPanel);
        viewport.addChangeListener(this);
        croppedEdge = new CroppedEdge(ui);
        createButtons(ui);
    }

    /**
     * Recreates the scroll buttons and adds them to the TabbedPane.
     */
    void createButtons(final DarkTabbedPaneUIBridge ui) {
        if (scrollForwardButton != null) {
            ui.tabPane.remove(scrollForwardButton);
            scrollForwardButton.removeActionListener(this);
            ui.tabPane.remove(scrollBackwardButton);
            scrollBackwardButton.removeActionListener(this);
        }
        int tabPlacement = ui.tabPane.getTabPlacement();
        if (tabPlacement == SwingConstants.TOP || tabPlacement == SwingConstants.BOTTOM) {
            scrollForwardButton = ui.createScrollButton(SwingConstants.EAST);
            scrollBackwardButton = ui.createScrollButton(SwingConstants.WEST);

        } else { // tabPlacement = LEFT || RIGHT
            scrollForwardButton = ui.createScrollButton(SwingConstants.SOUTH);
            scrollBackwardButton = ui.createScrollButton(SwingConstants.NORTH);
        }
        scrollForwardButton.addActionListener(this);
        scrollBackwardButton.addActionListener(this);
        ui.tabPane.add(scrollForwardButton);
        ui.tabPane.add(scrollBackwardButton);
    }

    public void scrollForward(final int tabPlacement) {
        Dimension viewSize = viewport.getViewSize();
        Rectangle viewRect = viewport.getViewRect();

        if (tabPlacement == SwingConstants.TOP || tabPlacement == SwingConstants.BOTTOM) {
            if (viewRect.width >= viewSize.width - viewRect.x) {
                return; // no room left to scroll
            }
        } else { // tabPlacement == LEFT || tabPlacement == RIGHT
            if (viewRect.height >= viewSize.height - viewRect.y) {
                return;
            }
        }
        setLeadingTabIndex(tabPlacement, leadingTabIndex + 1);
    }

    public void setLeadingTabIndex(final int tabPlacement, final int index) {
        leadingTabIndex = index;
        Dimension viewSize = viewport.getViewSize();
        Rectangle viewRect = viewport.getViewRect();

        switch (tabPlacement) {
            case SwingConstants.TOP:
            case SwingConstants.BOTTOM:
                tabViewPosition.x = leadingTabIndex == 0 ? 0 : ui.rects[leadingTabIndex].x;

                if ((viewSize.width - tabViewPosition.x) < viewRect.width) {
                    // We've scrolled to the end, so adjust the viewport size
                    // to ensure the view position remains aligned on a tab boundary
                    Dimension extentSize = new Dimension(viewSize.width - tabViewPosition.x,
                                                         viewRect.height);
                    viewport.setExtentSize(extentSize);
                }
                break;
            case SwingConstants.LEFT:
            case SwingConstants.RIGHT:
                tabViewPosition.y = leadingTabIndex == 0 ? 0 : ui.rects[leadingTabIndex].y;

                if ((viewSize.height - tabViewPosition.y) < viewRect.height) {
                    // We've scrolled to the end, so adjust the viewport size
                    // to ensure the view position remains aligned on a tab boundary
                    Dimension extentSize = new Dimension(viewRect.width,
                                                         viewSize.height - tabViewPosition.y);
                    viewport.setExtentSize(extentSize);
                }
        }
        viewport.setViewPosition(tabViewPosition);
    }

    public void scrollBackward(final int tabPlacement) {
        if (leadingTabIndex == 0) {
            return; // no room left to scroll
        }
        setLeadingTabIndex(tabPlacement, leadingTabIndex - 1);
    }

    public void stateChanged(final ChangeEvent e) {
        updateView();
    }

    protected void updateView() {
        int tabPlacement = ui.tabPane.getTabPlacement();
        int tabCount = ui.tabPane.getTabCount();
        ui.assureRectsCreated(tabCount);
        Rectangle vpRect = viewport.getBounds();
        Dimension viewSize = viewport.getViewSize();
        Rectangle viewRect = viewport.getViewRect();

        leadingTabIndex = ui.getClosestTab(viewRect.x, viewRect.y);

        // If the tab isn't right aligned, adjust it.
        if (leadingTabIndex + 1 < tabCount) {
            switch (tabPlacement) {
                case SwingConstants.TOP:
                case SwingConstants.BOTTOM:
                    if (ui.rects[leadingTabIndex].x < viewRect.x) {
                        leadingTabIndex++;
                    }
                    break;
                case SwingConstants.LEFT:
                case SwingConstants.RIGHT:
                    if (ui.rects[leadingTabIndex].y < viewRect.y) {
                        leadingTabIndex++;
                    }
                    break;
            }
        }
        Insets contentInsets = ui.getContentBorderInsets(tabPlacement);
        switch (tabPlacement) {
            case SwingConstants.LEFT:
                ui.tabPane.repaint(vpRect.x + vpRect.width, vpRect.y,
                                   contentInsets.left, vpRect.height);
                scrollBackwardButton.setEnabled(
                        viewRect.y > 0 && leadingTabIndex > 0);
                scrollForwardButton.setEnabled(
                        leadingTabIndex < tabCount - 1 &&
                                viewSize.height - viewRect.y > viewRect.height);
                break;
            case SwingConstants.RIGHT:
                ui.tabPane.repaint(vpRect.x - contentInsets.right, vpRect.y,
                                   contentInsets.right, vpRect.height);
                scrollBackwardButton.setEnabled(
                        viewRect.y > 0 && leadingTabIndex > 0);
                scrollForwardButton.setEnabled(
                        leadingTabIndex < tabCount - 1 &&
                                viewSize.height - viewRect.y > viewRect.height);
                break;
            case SwingConstants.BOTTOM:
                ui.tabPane.repaint(vpRect.x, vpRect.y - contentInsets.bottom,
                                   vpRect.width, contentInsets.bottom);
                scrollBackwardButton.setEnabled(
                        viewRect.x > 0 && leadingTabIndex > 0);
                scrollForwardButton.setEnabled(
                        leadingTabIndex < tabCount - 1 &&
                                viewSize.width - viewRect.x > viewRect.width);
                break;
            case SwingConstants.TOP:
            default:
                ui.tabPane.repaint(vpRect.x, vpRect.y + vpRect.height,
                                   vpRect.width, contentInsets.top);
                scrollBackwardButton.setEnabled(
                        viewRect.x > 0 && leadingTabIndex > 0);
                scrollForwardButton.setEnabled(
                        leadingTabIndex < tabCount - 1 &&
                                viewSize.width - viewRect.x > viewRect.width);
        }
    }

    /**
     * ActionListener for the scroll buttons.
     */
    public void actionPerformed(final ActionEvent e) {
        ActionMap map = ui.tabPane.getActionMap();

        if (map != null) {
            String actionKey;

            if (e.getSource() == scrollForwardButton) {
                actionKey = "scrollTabsForwardAction";
            } else {
                actionKey = "scrollTabsBackwardAction";
            }
            Action action = map.get(actionKey);

            if (action != null && action.isEnabled()) {
                action.actionPerformed(new ActionEvent(ui.tabPane,
                                                       ActionEvent.ACTION_PERFORMED, null, e.getWhen(),
                                                       e.getModifiers()));
            }
        }
    }

    public String toString() {
        return "viewport.viewSize=" + viewport.getViewSize() + "\n" +
                "viewport.viewRectangle=" + viewport.getViewRect() + "\n" +
                "leadingTabIndex=" + leadingTabIndex + "\n" +
                "tabViewPosition=" + tabViewPosition;
    }

}
