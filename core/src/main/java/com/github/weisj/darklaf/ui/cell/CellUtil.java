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
package com.github.weisj.darklaf.ui.cell;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ListUI;

import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.ui.table.DarkTableCellEditor;
import com.github.weisj.darklaf.util.DarkUIUtil;

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

    public static void paintTableEditorBorder(final Graphics g, final Component c, final JTable table,
                                              final int width, final int height) {
        int row = table.getEditingRow();
        int col = table.getEditingColumn();
        if (!table.getShowHorizontalLines()) {
            if (row > CellUtil.getMinRowIndex(table)) g.fillRect(0, 0, width, 1);
            g.fillRect(0, height - 1, width, 1);
        }
        if (!table.getShowVerticalLines()) {
            if (col > CellUtil.getMinColumnIndex(table)) g.fillRect(0, 0, 1, height);
            if (col < CellUtil.getMaxColumnIndex(table)) g.fillRect(width - 1, 0, 1, height);
        } else if (isInWrapper(c)) {
            if (table.getComponentOrientation().isLeftToRight()) {
                g.fillRect(0, 0, 1, height);
            } else {
                g.fillRect(width - 1, 0, 1, height);
            }
        }
    }

    protected static boolean isInWrapper(final Component c) {
        return c.getParent() instanceof DarkTableCellEditor.IconWrapper;
    }

    protected static boolean isListEditor(final Component c) {
        return c instanceof JComponent
               && Boolean.TRUE.equals(((JComponent) c).getClientProperty(DarkListUI.KEY_IS_LIST_RENDERER))
               && c.getParent() instanceof JList;
    }

    public static Insets adjustEditorInsets(final Insets ins, final Component c) {
        if (isInWrapper(c)) {
            if (parentLTR(c)) {
                ins.left -= ((DarkTableCellEditor.IconWrapper) c.getParent()).getIconCompGap();
            } else {
                ins.right -= ((DarkTableCellEditor.IconWrapper) c.getParent()).getIconCompGap();
            }
        } else if (isListEditor(c)) {
            ListCellRenderer<?> renderer = ((JList<?>) c.getParent()).getCellRenderer();
            if (renderer instanceof JLabel) {
                if (parentLTR(c)) {
                    ins.left -= ((JLabel) renderer).getIconTextGap() - 1;
                } else {
                    ins.right -= ((JLabel) renderer).getIconTextGap() - 1;
                }
            }
        }
        return adjustTableCellEditorInsets(ins, DarkUIUtil.getParentOfType(JTable.class, c));
    }

    public static Insets adjustTableCellEditorInsets(final Insets ins, final JTable table) {
        if (table != null && !table.getShowVerticalLines()) {
            int cMin = getMinColumnIndex(table);
            int column = table.getEditingColumn();
            if (column > cMin) ins.left++;
        }
        return ins;
    }

    protected static boolean parentLTR(final Component c) {
        return c.getParent().getComponentOrientation().isLeftToRight();
    }

    public static int getMinColumnIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        return table.columnAtPoint(rect.getLocation());
    }

    public static int getMaxColumnIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        Point p = rect.getLocation();
        p.x += rect.width - 1;
        return table.columnAtPoint(p);
    }

    public static int getMinRowIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        return table.rowAtPoint(rect.getLocation());
    }
}
