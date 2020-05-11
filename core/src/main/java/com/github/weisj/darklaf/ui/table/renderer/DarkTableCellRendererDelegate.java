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
package com.github.weisj.darklaf.ui.table.renderer;

import java.awt.*;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.github.weisj.darklaf.delegate.TableCellRendererDelegate;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererCheckBox;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererRadioButton;
import com.github.weisj.darklaf.ui.table.DarkTableCellFocusBorder;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TableConstants;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTableCellRendererDelegate extends TableCellRendererDelegate implements TableConstants {

    private final DarkCellRendererCheckBox checkBoxRenderer = new DarkCellRendererCheckBox();
    private final DarkCellRendererRadioButton radioRenderer = new DarkCellRendererRadioButton();

    public DarkTableCellRendererDelegate(final TableCellRenderer renderer) {
        super(renderer);
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value,
                                                   final boolean isSelected, final boolean hasFocus,
                                                   final int row, final int column) {
        TableCellRenderer renderer = TableConstants.useBooleanEditorForValue(value, table)
                ? getBooleanRenderer(table)
                : super.getDelegate();
        Component component = renderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        int horizontalAlignment = table.getComponentOrientation().isLeftToRight()
                ? SwingConstants.LEFT
                : SwingConstants.RIGHT;

        if (component instanceof JLabel) {
            ((JLabel) component).setVerticalAlignment(SwingConstants.CENTER);
            ((JLabel) component).setHorizontalAlignment(horizontalAlignment);
        } else if (component instanceof AbstractButton) {
            ((AbstractButton) component).setVerticalAlignment(SwingConstants.CENTER);
            ((AbstractButton) component).setHorizontalAlignment(horizontalAlignment);
        }

        boolean isRowFocus = DarkTableCellFocusBorder.isRowFocusBorder(table);
        boolean isLeadSelectionCell = DarkUIUtil.hasFocus(table) && hasFocus && !isRowFocus;
        boolean paintSelected = isSelected && !isLeadSelectionCell && !table.isEditing();

        if (component instanceof JComponent) {
            setupBorderStyle(table, row, column, (JComponent) component, isRowFocus);
        }
        CellUtil.setupTableForeground(component, table, paintSelected);
        CellUtil.setupTableBackground(component, table, paintSelected, row);
        return component;
    }

    public void setupBorderStyle(final JTable table, final int row, final int column,
                                 final JComponent component, final boolean isRowFocus) {
        if (isRowFocus
            && table.getSelectionModel().getLeadSelectionIndex() == row
            && DarkUIUtil.hasFocus(table)
            && !table.isEditing()) {
            LookAndFeel.installBorder(component, "Table.focusSelectedCellHighlightBorder");
            component.putClientProperty(KEY_FULL_ROW_FOCUS_BORDER, true);
            JTableHeader header = table.getTableHeader();
            TableColumn draggedColumn = (header == null) ? null : header.getDraggedColumn();
            boolean forceLeft = false;
            boolean forceRight = false;
            if (draggedColumn != null) {
                int index = DarkTableUI.viewIndexForColumn(draggedColumn, table);
                forceLeft = column == index + 1 || column == index;
                forceRight = column == index - 1 || column == index;
            }
            component.putClientProperty(KEY_FORCE_RIGHT_BORDER, forceRight);
            component.putClientProperty(KEY_FORCE_LEFT_BORDER, forceLeft);
        } else {
            component.putClientProperty(KEY_FULL_ROW_FOCUS_BORDER, false);
        }
    }

    protected TableCellRenderer getBooleanRenderer(final JTable table) {
        if (PropertyUtil.isPropertyEqual(table, KEY_BOOLEAN_RENDER_TYPE, RENDER_TYPE_RADIOBUTTON)) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
