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

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.ContainerEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;

public class DarkHandler extends TabbedPaneHandler {

    protected DarkTabbedPaneUI ui;

    protected Point origin;
    protected Point tabOrigin;
    protected int pressedIndex;

    protected DarkHandler(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent e) {
        super.propertyChange(e);
        var key = e.getPropertyName();
        if ("TabbedPane.maxPopupHeight".equals(key)) {
            Integer newVal = (Integer) e.getNewValue();
            if (newVal != null && newVal >= 0) {
                ui.scrollableTabSupport.scrollPopupMenu.setMaxHeight(newVal);
            }
        } else if ("JTabbedPane.tabAreaInsets".equals(key)) {
            var ins = e.getNewValue();
            if (ins instanceof Insets) {
                ui.tabAreaInsets = (Insets) ins;
            } else if (ins == null) {
                ui.tabAreaInsets = new Insets(0, 0, 0, 0);
            }
        } else if ("JTabbedPane.contentBorderInsets".equals(key)) {
            var ins = e.getNewValue();
            if (ins instanceof Insets) {
                ui.contentBorderInsets = (Insets) ins;
            } else if (ins == null) {
                ui.contentBorderInsets = new Insets(0, 0, 0, 0);
            }
        } else if ("tabPlacement".equals(key)) {
            if (ui.scrollableTabLayoutEnabled()) {
                ui.currentShiftX = 0;
                ui.currentShiftY = 0;
                ui.scrollLayout.calculateTabRects(ui.tabPane.getTabPlacement(), ui.tabPane.getTabCount());
            }
        } else if ("JTabbedPane.showNewTabButton".equals(key)) {
            var val = e.getNewValue();
            if (val instanceof Boolean && ui.scrollableTabLayoutEnabled()) {
                boolean show = (Boolean) val;
                if (show == ui.scrollableTabSupport.newTabButton.isVisible()) {
                    return;
                }
                ui.scrollableTabSupport.newTabButton.setVisible(show);
            }
        } else if ("JTabbedPane.leadingComponent".equals(key)) {
            ui.tabPane.remove(ui.leadingComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.leadingComp = (Component) val;
                ui.tabPane.add(ui.leadingComp);
            } else {
                ui.leadingComp = null;
            }
        } else if ("JTabbedPane.trailingComponent".equals(key)) {
            ui.tabPane.remove(ui.trailingComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.trailingComp = (Component) val;
                ui.tabPane.add(ui.trailingComp);
            } else {
                ui.trailingComp = null;
            }
        } else if ("JTabbedPane.dndEnabled".equals(key)) {
            ui.dndEnabled = Boolean.TRUE.equals(ui.tabPane.getClientProperty("JTabbedPane.dndEnabled"));
            ui.tabPane.getDropTarget().setActive(ui.dndEnabled);
        } else if ("componentOrientation".equals(key)) {
            ui.tabPane.doLayout();
            ui.tabPane.repaint();
        } else if ("JTabbedPane.northComponent".equals(key)) {
            ui.tabPane.remove(ui.northComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.northComp = (Component) val;
                ui.tabPane.add(ui.northComp);
            } else {
                ui.northComp = null;
            }
        } else if ("JTabbedPane.southComponent".equals(key)) {
            ui.tabPane.remove(ui.southComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.southComp = (Component) val;
                ui.tabPane.add(ui.southComp);
            } else {
                ui.southComp = null;
            }
        } else if ("JTabbedPane.eastComponent".equals(key)) {
            ui.tabPane.remove(ui.eastComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.eastComp = (Component) val;
                ui.tabPane.add(ui.eastComp);
            } else {
                ui.eastComp = null;
            }
        } else if ("JTabbedPane.westComponent".equals(key)) {
            ui.tabPane.remove(ui.westComp);
            var val = e.getNewValue();
            if (val instanceof Component) {
                ui.westComp = (Component) val;
                ui.tabPane.add(ui.westComp);
            } else {
                ui.westComp = null;
            }
        }
    }

    public void stateChanged(@NotNull final ChangeEvent e) {
        JTabbedPane tabPane = (JTabbedPane) e.getSource();
        tabPane.revalidate();
        tabPane.repaint();
        ui.setFocusIndex(tabPane.getSelectedIndex(), false);
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        super.mousePressed(e);
        ui.tabPane.requestFocus();
        origin = e.getPoint();
        pressedIndex = ui.tabForCoordinate(ui.tabPane, e.getX(), e.getY());
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
                var margins = ui.scrollLayout.getMargins(ui.tabPane.getTabPlacement());
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
                var p = getDragMousePos();
                int tab = TabbedPaneUtil.getDroppedTabIndex(ui.dropRect, ui.tabPane, ui, p);
                var rect = TabbedPaneUtil.getDropRect(ui, ui.tabPane, ui.tabPane, p,
                                                      ui.dropRect, tab, ui.dropSourceIndex, ui.dropTargetIndex);
                ui.setDnDIndicatorRect(rect.x, rect.y, rect.width, rect.height, tab, true);
            }
        }

        if (indexValid) {
            var p = e.getPoint();
            int dist = Math.abs(ui.isHorizontalTabPlacement() ? origin.y - p.y : origin.x - p.x);
            if (dist > Math.max(50, ui.maxTabHeight)) {
                stopDrag(e);
                TransferHandler handler = ui.tabPane.getTransferHandler();
                handler.exportAsDrag(ui.tabPane, e, TransferHandler.MOVE);
            }
        }
    }

    protected Point getDragMousePos() {
        var p = new Point(ui.dragRect.x + ui.dragRect.width / 2, ui.dragRect.y + ui.dragRect.height / 2);
        p.x += ui.scrollableTabSupport.viewport.getX();
        p.y += ui.scrollableTabSupport.viewport.getY();
        return p;
    }

    protected void stopDrag(final MouseEvent e) {
        int tab = TabbedPaneUtil.getDroppedTabIndex(ui.dropRect, ui.tabPane,
                                                    ui, getDragMousePos());
        if (tab >= 0 && tab <= ui.tabPane.getTabCount()) {
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
        ui.scrollableTabSupport.viewport.repaint();
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        super.mouseReleased(e);
        if (ui.dragging && ui.scrollableTabLayoutEnabled()) {
            stopDrag(e);
        }
    }

    @Override
    public void componentAdded(@NotNull final ContainerEvent e) {
        if (!(e.getChild() instanceof UIResource)) {
            e.getChild().addFocusListener(ui.focusListener);
        }
        super.componentAdded(e);
    }

    @Override
    public void componentRemoved(@NotNull final ContainerEvent e) {
        if (!(e.getChild() instanceof UIResource)) {
            e.getChild().removeFocusListener(ui.focusListener);
        }
        super.componentRemoved(e);
    }
}
