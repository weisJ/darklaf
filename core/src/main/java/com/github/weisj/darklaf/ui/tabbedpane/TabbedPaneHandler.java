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
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class TabbedPaneHandler implements ChangeListener, ContainerListener, FocusListener, MouseListener,
        MouseMotionListener, PropertyChangeListener {
    private final DarkTabbedPaneUIBridge ui;

    //
    // PropertyChangeListener
    //

    protected TabbedPaneHandler(final DarkTabbedPaneUIBridge ui) {
        this.ui = ui;
    }

    public void propertyChange(final PropertyChangeEvent e) {
        JTabbedPane pane = (JTabbedPane) e.getSource();
        String name = e.getPropertyName();
        boolean isScrollLayout = ui.scrollableTabLayoutEnabled();
        if (Objects.equals(name, "mnemonicAt")) {
            ui.updateMnemonics();
            pane.repaint();
        } else if (Objects.equals(name, "displayedMnemonicIndexAt")) {
            pane.repaint();
        } else if (Objects.equals(name, "indexForTitle")) {
            ui.calculatedBaseline = false;
            Integer index = (Integer) e.getNewValue();
            updateHtmlViews(index, false);
        } else if (Objects.equals(name, "tabLayoutPolicy")) {
            ui.uninstallUI(pane);
            ui.installUI(pane);
            ui.calculatedBaseline = false;
        } else if (Objects.equals(name, "tabPlacement")) {
            if (ui.scrollableTabLayoutEnabled()) {
                ui.tabScroller.createButtons(ui);
            }
            ui.calculatedBaseline = false;
        } else if (Objects.equals(name, PropertyKey.OPAQUE) && isScrollLayout) {
            boolean newVal = (Boolean) e.getNewValue();
            ui.tabScroller.tabPanel.setOpaque(newVal);
            ui.tabScroller.viewport.setOpaque(newVal);
        } else if (Objects.equals(name, PropertyKey.BACKGROUND) && isScrollLayout) {
            Color newVal = (Color) e.getNewValue();
            ui.tabScroller.tabPanel.setBackground(newVal);
            ui.tabScroller.viewport.setBackground(newVal);
        } else if (Objects.equals(name, "indexForTabComponent")) {
            if (ui.tabContainer != null) {
                ui.tabContainer.removeUnusedTabComponents();
            }
            Component c = ui.tabPane.getTabComponentAt((Integer) e.getNewValue());
            if (c != null) {
                if (ui.tabContainer == null) {
                    ui.installTabContainer();
                } else {
                    ui.tabContainer.add(c);
                }
            }
            ui.tabPane.revalidate();
            ui.tabPane.repaint();
            ui.calculatedBaseline = false;
        } else if (Objects.equals(name, "indexForNullComponent")) {
            ui.isRunsDirty = true;
            updateHtmlViews((Integer) e.getNewValue(), true);
        } else if (Objects.equals(name, PropertyKey.FONT) || DarkUIUtil.isScaleChanged(e)) {
            ui.calculatedBaseline = false;
        }
    }

    protected void updateHtmlViews(final int index, final boolean inserted) {
        String title = ui.tabPane.getTitleAt(index);
        boolean isHTML = BasicHTML.isHTMLString(title);
        if (isHTML) {
            if (ui.htmlViews == null) { // Initialize vector
                ui.htmlViews = ui.createHTMLVector();
            } else { // Vector already exists
                View v = BasicHTML.createHTMLView(ui.tabPane, title);
                setHtmlView(v, inserted, index);
            }
        } else { // Not HTML
            if (ui.htmlViews != null) { // Add placeholder
                setHtmlView(null, inserted, index);
            } // else nada!
        }
        ui.updateMnemonics();
    }

    protected void setHtmlView(final View v, final boolean inserted, final int index) {
        if (inserted || index >= ui.htmlViews.size()) {
            ui.htmlViews.insertElementAt(v, index);
        } else {
            ui.htmlViews.setElementAt(v, index);
        }
    }

    //
    // ChangeListener
    //
    public void stateChanged(final ChangeEvent e) {
        JTabbedPane tabPane = (JTabbedPane) e.getSource();
        tabPane.revalidate();
        tabPane.repaint();

        ui.setFocusIndex(tabPane.getSelectedIndex(), false);

        if (ui.scrollableTabLayoutEnabled()) {
            ui.ensureCurrentLayout();
            int index = tabPane.getSelectedIndex();
            if (index < ui.rects.length && index != -1) {
                ui.tabScroller.tabPanel.scrollRectToVisible((Rectangle) ui.rects[index].clone());
            }
        }
    }

    //
    // MouseListener
    //
    public void mouseClicked(final MouseEvent e) {}

    public void mousePressed(final MouseEvent e) {
        if (!ui.tabPane.isEnabled()) {
            return;
        }
        int tabIndex = ui.tabForCoordinate(ui.tabPane, e.getX(), e.getY());
        if (tabIndex >= 0 && ui.tabPane.isEnabledAt(tabIndex)) {
            if (tabIndex != ui.tabPane.getSelectedIndex()) {
                // Clicking on unselected tab, change selection, do NOT
                // request focus.
                // This will trigger the focusIndex to change by way
                // of stateChanged.
                ui.tabPane.setSelectedIndex(tabIndex);
            } else if (ui.tabPane.isRequestFocusEnabled()) {
                // Clicking on selected tab, try and give the tabbedpane
                // focus. Repaint will occur in focusGained.
                ui.tabPane.requestFocusInWindow();
            }
        }
    }

    public void mouseReleased(final MouseEvent e) {}

    public void mouseEntered(final MouseEvent e) {
        ui.setRolloverTab(e.getX(), e.getY());
    }

    public void mouseExited(final MouseEvent e) {
        ui.setRolloverTab(-1);
    }

    //
    // MouseMotionListener
    //
    public void mouseDragged(final MouseEvent e) {}

    public void mouseMoved(final MouseEvent e) {
        ui.setRolloverTab(e.getX(), e.getY());
    }

    //
    // FocusListener
    //
    public void focusGained(final FocusEvent e) {
        ui.setFocusIndex(ui.tabPane.getSelectedIndex(), true);
    }

    public void focusLost(final FocusEvent e) {
        ui.repaintTab(ui.focusIndex);
    }

    //
    // ContainerListener
    //
    /*
     * GES 2/3/99: The container listener code was added to support HTML rendering of tab titles.
     *
     * Ideally, we would be able to listen for property changes when a tab is added or its text
     * modified. At the moment there are no such events because the Beans spec doesn't allow 'indexed'
     * property changes (i.e. tab 2's text changed from A to B).
     *
     * In order to get around this, we listen for tabs to be added or removed by listening for the
     * container events. we then queue up a runnable (so the component has a chance to complete the add)
     * which checks the tab title of the new component to see if it requires HTML rendering.
     *
     * The Views (one per tab title requiring HTML rendering) are stored in the htmlViews Vector, which
     * is only allocated after the first time we run into an HTML tab. Note that this vector is kept in
     * step with the number of pages, and nulls are added for those pages whose tab title do not require
     * HTML rendering.
     *
     * This makes it easy for the paint and layout code to tell whether to invoke the HTML engine
     * without having to check the string during time-sensitive operations.
     *
     * When we have added a way to listen for tab additions and changes to tab text, this code should be
     * removed and replaced by something which uses that.
     */
    public void componentAdded(final ContainerEvent e) {
        JTabbedPane tp = (JTabbedPane) e.getContainer();
        Component child = e.getChild();
        if (child instanceof UIResource) {
            return;
        }
        ui.isRunsDirty = true;
        updateHtmlViews(tp.indexOfComponent(child), true);
    }

    public void componentRemoved(final ContainerEvent e) {
        JTabbedPane tp = (JTabbedPane) e.getContainer();
        Component child = e.getChild();
        if (child instanceof UIResource) {
            return;
        }

        // NOTE 4/15/2002 (joutwate):
        // This fix is implemented using client properties since there is
        // currently no IndexPropertyChangeEvent. Once
        // IndexPropertyChangeEvents have been added this code should be
        // modified to use it.
        int index = PropertyUtil.getInteger(tp, "__index_to_remove__", -1);
        if (index >= 0) {
            if (ui.htmlViews != null && ui.htmlViews.size() > index) {
                ui.htmlViews.removeElementAt(index);
            }
            tp.putClientProperty("__index_to_remove__", null);
        }
        ui.isRunsDirty = true;
        ui.updateMnemonics();

        ui.validateFocusIndex();
    }
}
