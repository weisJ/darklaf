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
 *
 */
package com.github.weisj.darklaf.ui.combobox;

import java.awt.*;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.*;
import javax.swing.plaf.basic.BasicComboPopup;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.ui.scrollpane.DarkScrollBarUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

/**
 * @author Jannis Weis
 */
public class DarkComboPopup extends BasicComboPopup {

    private final AdjustmentListener adjustmentListener = e -> {
        Point p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, list);
        MouseEvent ev = new MouseEvent(list,
                                       MouseEvent.MOUSE_MOVED, 0, 0, p.x, p.y, 1,
                                       false, 0);
        for (MouseMotionListener ml : list.getMouseMotionListeners()) {
            if (ml != null) {
                ml.mouseMoved(ev);
            }
        }
    };
    private final int borderSize;
    private double lastEvent;
    private boolean visible = false;
    private OverlayScrollPane overlayScrollPane;

    /**
     * Constructs a new instance of {@code BasicComboPopup}.
     *
     * @param combo      an instance of {@code JComboBox}
     * @param borderSize the size of the border
     */
    public DarkComboPopup(final JComboBox<Object> combo, final int borderSize) {
        super(combo);
        this.borderSize = borderSize;
    }

    @Override
    protected JList<Object> createList() {
        JList<Object> list = super.createList();
        list.putClientProperty(DarkListUI.KEY_IS_COMBO_LIST, true);
        return list;
    }

    @Override
    protected void firePopupMenuWillBecomeVisible() {
        if (list.getModel().getSize() != 0) {
            int height = list.getUI().getCellBounds(list, 0, 0).height;
            overlayScrollPane.getVerticalScrollBar().setUnitIncrement(height);
        }
        visible = true;
        overlayScrollPane.getVerticalScrollBar().addAdjustmentListener(adjustmentListener);
        super.firePopupMenuWillBecomeVisible();
    }

    @Override
    protected void firePopupMenuWillBecomeInvisible() {
        lastEvent = System.currentTimeMillis();
        visible = false;
        overlayScrollPane.getVerticalScrollBar().removeAdjustmentListener(adjustmentListener);
        super.firePopupMenuWillBecomeInvisible();
    }

    @Override
    protected void firePopupMenuCanceled() {
        visible = false;
        overlayScrollPane.getVerticalScrollBar().removeAdjustmentListener(adjustmentListener);
        super.firePopupMenuCanceled();
    }

    @Override
    protected JScrollPane createScroller() {
        overlayScrollPane = new OverlayScrollPane(list, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                  ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        overlayScrollPane.getVerticalScrollBar().putClientProperty(DarkScrollBarUI.KEY_SMALL, Boolean.TRUE);
        return overlayScrollPane.getScrollPane();
    }

    @Override
    protected void configureScroller() {
        scroller.setFocusable(false);
        scroller.getVerticalScrollBar().setFocusable(false);
        scroller.setBorder(null);
    }

    @Override
    protected void configurePopup() {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorderPainted(true);
        setOpaque(false);
        add(overlayScrollPane);
        setFocusable(false);
    }

    @Override
    public void show() {
        comboBox.firePopupMenuWillBecomeVisible();
        setListSelection(comboBox.getSelectedIndex());
        Point location = getPopupLocation();
        show(comboBox, location.x, location.y);
    }

    protected void setListSelection(final int selectedIndex) {
        if (selectedIndex == -1) {
            list.clearSelection();
        } else {
            list.setSelectedIndex(selectedIndex);
            list.ensureIndexIsVisible(selectedIndex);
        }
    }

    protected Point getPopupLocation() {
        Dimension popupSize = comboBox.getSize();
        Insets insets = getInsets();

        // reduce the width of the scrollpane by the insets so that the popup
        // is the same width as the combo box.
        popupSize.setSize(popupSize.width - (insets.right + insets.left),
                          getPopupHeightForRowCount(comboBox.getMaximumRowCount()));
        Rectangle popupBounds = computePopupBounds(borderSize, comboBox.getBounds().height - borderSize,
                                                   popupSize.width - 2 * borderSize, popupSize.height);
        Dimension scrollSize = popupBounds.getSize();
        Point popupLocation = popupBounds.getLocation();

        scroller.setMaximumSize(scrollSize);
        scroller.setPreferredSize(scrollSize);
        scroller.setMinimumSize(scrollSize);

        list.revalidate();

        return popupLocation;
    }

    protected Rectangle computePopupBounds(final int px, final int py, final int pw, final int ph) {
        Rectangle screenBounds = DarkUIUtil.getScreenBounds(comboBox, null);

        Point relativeOrigin = new Point();
        SwingUtilities.convertPointFromScreen(relativeOrigin, comboBox);
        screenBounds.setLocation(relativeOrigin);

        Rectangle rect = new Rectangle(px, py, pw, ph);
        if (py + ph > screenBounds.y + screenBounds.height) {
            if (ph <= -screenBounds.y) {
                // popup goes above
                rect.y = -ph + borderSize;
            } else {
                // a full screen height popup
                rect.y = screenBounds.y + Math.max(0, (screenBounds.height - ph) / 2);
                rect.height = Math.min(screenBounds.height, ph);
            }
        }
        return rect;
    }

    @Override
    protected void togglePopup() {
        if (comboBox.getItemCount() == 0) return;
        if (visible) {
            visible = false;
            hide();
        } else if (lastEvent == 0 || (System.currentTimeMillis() - lastEvent) > 250) {
            visible = true;
            SwingUtilities.invokeLater(this::show);
        }
    }

    protected void reset() {
        lastEvent = 0;
        if (visible) {
            hide();
        }
        visible = false;
    }
}
