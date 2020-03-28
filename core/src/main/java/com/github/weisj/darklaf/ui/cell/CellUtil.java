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
 */
package com.github.weisj.darklaf.ui.cell;

import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.plaf.ListUI;
import java.awt.*;

public class CellUtil {

    public static final String KEY_SELECTED_CELL_RENDERER = "JComponent.selectedCellRenderer";

    public static void setupForeground(final Component comp, final JComponent parent, final boolean selected,
                                       final String activeKey, final String inactiveKey) {
        setupForeground(comp, parent, selected, UIManager.getColor(activeKey), inactiveKey);
    }

    public static void setupForeground(final Component comp, final JTable parent, final boolean selected,
                                       final String inactiveKey) {
        setupForeground(comp, parent, selected, parent.getSelectionForeground(), inactiveKey);
    }

    public static void setupForeground(final Component comp, final JComponent parent, final boolean selected,
                                       final Color activeColor, final String inactiveKey) {
        if (selected) {
            if (DarkUIUtil.hasFocus(parent)) {
                comp.setForeground(activeColor);
            } else {
                comp.setForeground(UIManager.getColor(inactiveKey));
            }
        } else {
            comp.setForeground(parent.getForeground());
        }
        setSelectedFlag(comp, selected);
    }

    public static void setSelectedFlag(final Component comp, final boolean selected) {
        if (comp instanceof JComponent) {
            ((JComponent) comp).putClientProperty(KEY_SELECTED_CELL_RENDERER, selected);
        }
    }

    public static void setupBackground(final Component comp, final JTable parent,
                                       final boolean selected, final int row,
                                       final String altBgKey, final String altColorKey,
                                       final String noFocusSelectionBgKey) {
        setupBackground(comp, parent, selected, row, altBgKey, parent.getBackground(), altColorKey,
                        parent.getSelectionBackground(), noFocusSelectionBgKey);
    }

    public static void setupBackground(final Component comp, final JTable parent,
                                       final boolean selected, final int row,
                                       final String altBgKey, final String colorKey, final String altColorKey,
                                       final String noFocusSelectionBgKey) {
        setupBackground(comp, parent, selected, row, altBgKey, UIManager.getColor(colorKey),
                        altColorKey, parent.getSelectionBackground(), noFocusSelectionBgKey);
    }

    public static void setupBackground(final Component comp, final JList<?> parent, final boolean selected,
                                       final int index, final String altBgKey, final String altColorKey,
                                       final String noFocusSelectionBgKey) {
        int layout = parent.getLayoutOrientation();
        boolean altRow = false;
        if (layout == JList.VERTICAL) {
            altRow = index % 2 == 1;
        } else if (layout == JList.VERTICAL_WRAP || layout == JList.HORIZONTAL_WRAP) {
            ListUI ui = parent.getUI();
            if (ui instanceof DarkListUI) {
                int row = ((DarkListUI) ui).convertModelToRow(index);
                altRow = row % 2 == 1;
            } else {
                altRow = false;
            }
        }
        setupBackground(comp, parent, selected, altRow ? 1 : 0, altBgKey, parent.getBackground(),
                        altColorKey, parent.getSelectionBackground(), noFocusSelectionBgKey);
    }

    protected static void setupBackground(final Component comp, final JComponent parent,
                                          final boolean selected, final int row,
                                          final String altBgKey, final Color bgColor, final String altColorKey,
                                          final Color selectionBackground, final String noFocusSelectionBgKey) {
        boolean alternativeRow = Boolean.TRUE.equals(parent.getClientProperty(altBgKey));
        Color alternativeRowColor = UIManager.getColor(altColorKey);
        Color background = alternativeRow && row % 2 == 1 ? alternativeRowColor : bgColor;
        if (selected) {
            if (DarkUIUtil.hasFocus(parent)) {
                comp.setBackground(selectionBackground);
            } else {
                comp.setBackground(UIManager.getColor(noFocusSelectionBgKey));
            }
        } else {
            comp.setBackground(background);
        }
    }
}
