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
import java.awt.dnd.DropTarget;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.event.ChangeEvent;

import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTabbedPaneHandler extends TabbedPaneHandler {

    protected final DarkTabbedPaneUI ui;

    protected Point origin;
    protected Point tabOrigin;
    protected int pressedIndex;

    protected DarkTabbedPaneHandler(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        String key = e.getPropertyName();
        if (DarkTabbedPaneUI.KEY_TAB_AREA_INSETS.equals(key)) {
            Object ins = e.getNewValue();
            if (ins instanceof Insets) {
                ui.tabAreaInsets = (Insets) ins;
            } else if (ins == null) {
                ui.tabAreaInsets = new Insets(0, 0, 0, 0);
            }
        } else if (DarkTabbedPaneUI.KEY_CONTENT_BORDER_INSETS.equals(key)) {
            Object ins = e.getNewValue();
            if (ins instanceof Insets) {
                ui.contentBorderInsets = (Insets) ins;
            } else if (ins == null) {
                ui.contentBorderInsets = new Insets(0, 0, 0, 0);
            }
        } else if (DarkTabbedPaneUI.KEY_TAB_PLACEMENT.equals(key)) {
            if (ui.scrollableTabLayoutEnabled()) {
                ui.currentShiftX = 0;
                ui.currentShiftY = 0;
                ui.scrollLayout.calculateTabRects(ui.tabPane.getTabPlacement(), ui.tabPane.getTabCount());
            }
        } else if (DarkTabbedPaneUI.KEY_SHOW_NEW_TAB_BUTTON.equals(key)) {
            Object val = e.getNewValue();
            if (val instanceof Boolean && ui.scrollableTabLayoutEnabled()) {
                boolean show = (Boolean) val;
                if (show == ui.scrollableTabSupport.newTabButton.isVisible()) {
                    return;
                }
                ui.scrollableTabSupport.newTabButton.setVisible(show);
            }
            ui.tabPane.doLayout();
            ui.tabPane.repaint();
        } else if (DarkTabbedPaneUI.KEY_LEADING_COMP.equals(key)) {
            ui.tabPane.remove(ui.leadingComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.leadingComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.leadingComp);
            } else {
                ui.leadingComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_TRAILING_COMP.equals(key)) {
            ui.tabPane.remove(ui.trailingComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.trailingComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.trailingComp);
            } else {
                ui.trailingComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_DND.equals(key)) {
            ui.dndEnabled = PropertyUtil.getBooleanProperty(ui.tabPane, DarkTabbedPaneUI.KEY_DND);
            DropTarget dropTarget = ui.tabPane.getDropTarget();
            if (dropTarget != null) dropTarget.setActive(ui.dndEnabled);
        } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            ui.tabPane.doLayout();
            ui.tabPane.repaint();
        } else if (DarkTabbedPaneUI.KEY_NORTH_COMP.equals(key)) {
            ui.tabPane.remove(ui.northComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.northComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.northComp);
            } else {
                ui.northComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_SOUTH_COMP.equals(key)) {
            ui.tabPane.remove(ui.southComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.southComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.southComp);
            } else {
                ui.southComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_EAST_COMP.equals(key)) {
            ui.tabPane.remove(ui.eastComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.eastComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.eastComp);
            } else {
                ui.eastComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_WEST_COMP.equals(key)) {
            ui.tabPane.remove(ui.westComp);
            Object val = e.getNewValue();
            if (val instanceof Component) {
                ui.westComp = ui.wrapClientComponent((Component) val);
                ui.tabPane.add(ui.westComp);
            } else {
                ui.westComp = null;
            }
            ui.tabPane.doLayout();
        } else if (DarkTabbedPaneUI.KEY_CENTER_TABS.endsWith(key)) {
            ui.tabPane.doLayout();
            ui.tabPane.repaint();
        }
    }

    public void stateChanged(final ChangeEvent e) {
        JTabbedPane tabPane = (JTabbedPane) e.getSource();
        ui.setFocusIndex(tabPane.getSelectedIndex(), true);
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        super.mousePressed(e);
        ui.tabPane.requestFocusInWindow();
        origin = e.getPoint();
        pressedIndex = ui.tabForCoordinate(ui.tabPane, e.getX(), e.getY());
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        super.mouseReleased(e);
        if (ui.dragging && ui.scrollableTabLayoutEnabled()) {
            stopDrag(e, true);
        }
    }

    @Override
    public void mouseDragged(final MouseEvent e) {
        super.mouseDragged(e);
        if (!ui.dndEnabled) return;
        if (origin == null) {
            origin = e.getPoint();
            pressedIndex = ui.tabForCoordinate(ui.tabPane, e.getX(), e.getY());
        }
        boolean indexValid = pressedIndex >= 0 && pressedIndex < ui.tabPane.getTabCount();
        if (ui.scrollableTabLayoutEnabled()) {
            if (!ui.dragging && indexValid) {
                ui.dragRect.setBounds(ui.rects[pressedIndex]);
                tabOrigin = ui.rects[pressedIndex].getLocation();
                ui.dragging = true;
                ui.dropSourceIndex = pressedIndex;
                ui.dropTargetIndex = pressedIndex;
                ui.sourceEqualsTarget = true;
                ui.drawDropRect = true;
                ui.dropRect.setBounds(ui.rects[pressedIndex]);
                ui.dragRect.setBounds(ui.rects[pressedIndex]);
            } else if (ui.dragging && indexValid) {
                Point margins = ui.scrollLayout.getMargins(ui.tabPane.getTabPlacement());
                int min = margins.x;
                if (ui.isHorizontalTabPlacement()) {
                    int max = margins.y - ui.dropRect.width;
                    ui.dragRect.x = tabOrigin.x + e.getX() - origin.x;
                    ui.dragRect.x = Math.max(Math.min(ui.dragRect.x, max), min);
                } else {
                    int max = margins.y - ui.dropRect.height;
                    ui.dragRect.y = tabOrigin.x + e.getY() - origin.y;
                    ui.dragRect.x = Math.max(Math.min(ui.dragRect.y, max), min);
                }
                Point p = getDragMousePos();
                int tab = TabbedPaneUtil.getDroppedTabIndex(ui.dropRect, ui.tabPane, ui, p);
                Rectangle rect = TabbedPaneUtil.getDropRect(ui, ui.tabPane, ui.tabPane, p, ui.dropRect, tab,
                        ui.dropSourceIndex, ui.dropTargetIndex);
                ui.setDnDIndicatorRect(rect.x, rect.y, rect.width, rect.height, tab, true);
            }
        }

        if (indexValid) {
            Point p = e.getPoint();
            int dist = Math.abs(ui.isHorizontalTabPlacement() ? origin.y - p.y : origin.x - p.x);
            if (dist > Math.max(50, ui.maxTabHeight) || !ui.scrollableTabLayoutEnabled()) {
                stopDrag(e, false);
                TransferHandler handler = ui.tabPane.getTransferHandler();
                handler.exportAsDrag(ui.tabPane, e, TransferHandler.MOVE);
            }
        }
    }

    protected Point getDragMousePos() {
        Point p = new Point(ui.dragRect.x + ui.dragRect.width / 2, ui.dragRect.y + ui.dragRect.height / 2);
        if (ui.scrollableTabLayoutEnabled()) {
            p.x += ui.scrollableTabSupport.viewport.getX();
            p.y += ui.scrollableTabSupport.viewport.getY();
        }
        return p;
    }

    protected void stopDrag(final MouseEvent e, final boolean changeTabs) {
        int tab = TabbedPaneUtil.getDroppedTabIndex(ui.dropRect, ui.tabPane, ui, getDragMousePos());
        if (changeTabs && tab >= 0 && tab <= ui.tabPane.getTabCount()) {
            TabbedPaneUtil.moveTabs(ui.tabPane, ui.tabPane, ui.dropSourceIndex, tab);
        }
        SwingUtilities.invokeLater(() -> ui.setRolloverTab(e.getX(), e.getY()));

        ui.dragging = false;
        ui.dropRect.setBounds(0, 0, 0, 0);
        pressedIndex = -1;
        ui.dropTargetIndex = -1;
        ui.dropSourceIndex = -1;
        origin = null;
        ui.drawDropRect = false;
        ui.tabPane.doLayout();
        ui.tabPane.repaint();
        if (ui.scrollableTabLayoutEnabled()) {
            ui.scrollableTabSupport.viewport.repaint();
        } else {
            ui.tabPane.repaint();
        }
    }
}
