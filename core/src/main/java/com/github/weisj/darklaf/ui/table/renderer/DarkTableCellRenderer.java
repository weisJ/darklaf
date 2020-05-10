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
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.github.weisj.darklaf.ui.table.DarkTableCellFocusBorder;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellRenderer extends DefaultTableCellRenderer {

    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorCheckBox> checkBoxRenderer = new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox(true));
    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorRadioButton> radioRenderer = new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorRadioButton(true));

    protected static boolean isBooleanRenderingEnabled(final JTable table) {
        return PropertyUtil.getBooleanProperty(table, DarkTableUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX);
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value,
                                                   final boolean isSelected, final boolean hasFocus,
                                                   final int row, final int column) {
        if (value instanceof Boolean && isBooleanRenderingEnabled(table)) {
            return getBooleanRenderer(table).getTableCellRendererComponent(table, value, isSelected,
                                                                           hasFocus, row, column);
        }

        JComponent component = (JComponent) super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                                                                row, column);
        this.setVerticalAlignment(SwingConstants.CENTER);
        setHorizontalAlignment(table.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);

        boolean isRowFocus = DarkTableCellFocusBorder.isRowFocusBorder(table);
        boolean isLeadSelectionCell = DarkUIUtil.hasFocus(table) && hasFocus && !isRowFocus;
        boolean paintSelected = isSelected && !isLeadSelectionCell && !table.isEditing();

        setupBorderStyle(table, row, column, component, isRowFocus);
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
            component.setBorder(UIManager.getBorder("Table.focusSelectedCellHighlightBorder"));
            component.putClientProperty(DarkTableUI.KEY_FULL_ROW_FOCUS_BORDER, true);
            JTableHeader header = table.getTableHeader();
            TableColumn draggedColumn = (header == null) ? null : header.getDraggedColumn();
            boolean forceLeft = false;
            boolean forceRight = false;
            if (draggedColumn != null) {
                int index = DarkTableUI.viewIndexForColumn(draggedColumn, table);
                forceLeft = column == index + 1 || column == index;
                forceRight = column == index - 1 || column == index;
            }
            component.putClientProperty(DarkTableUI.KEY_FORCE_RIGHT_BORDER, forceRight);
            component.putClientProperty(DarkTableUI.KEY_FORCE_LEFT_BORDER, forceLeft);
        } else {
            component.putClientProperty(DarkTableUI.KEY_FULL_ROW_FOCUS_BORDER, false);
        }
    }

    protected TableCellRenderer getBooleanRenderer(final JTable table) {
        if (PropertyUtil.isPropertyEqual(table, DarkTableUI.KEY_BOOLEAN_RENDER_TYPE,
                                         DarkTableUI.RENDER_TYPE_RADIOBUTTON)) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
