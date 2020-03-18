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
package com.github.weisj.darklaf.ui.table;

import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellRenderer extends DefaultTableCellRenderer {

    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorCheckBox> checkBoxRenderer =
        new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox(true));
    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorRadioButton> radioRenderer =
        new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorRadioButton(true));

    protected static boolean isBooleanRenderingEnabled(final JTable table) {
        return Boolean.TRUE.equals(table.getClientProperty(DarkTableUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX));
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

        boolean isLeadSelectionCell = DarkUIUtil.hasFocus(table) && (hasFocus
            || (table.getSelectionModel().getLeadSelectionIndex() == row
            && DarkTableCellFocusBorder.isRowFocusBorder(table)));

        if (DarkTableCellFocusBorder.isRowFocusBorder(table)
            && table.getSelectionModel().getLeadSelectionIndex() == row
            && !table.isEditing()
            && DarkUIUtil.hasFocus(table)) {
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

        boolean alternativeRow = Boolean.TRUE.equals(table.getClientProperty(DarkTableUI.KEY_ALTERNATE_ROW_COLOR));
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = table.getBackground();
        Color background = alternativeRow && row % 2 == 1 ? alternativeRowColor : normalColor;
        if (!isSelected || isLeadSelectionCell || table.isEditing()) {
            component.setBackground(background);
            component.setForeground(table.getForeground());
        } else {
            if (DarkUIUtil.hasFocus(table)) {
                component.setForeground(table.getSelectionForeground());
            } else {
                component.setForeground(UIManager.getColor("Table.selectionForegroundInactive"));
            }
            component.setBackground(table.getSelectionBackground());
        }
        return component;
    }

    protected TableCellRenderer getBooleanRenderer(final JTable table) {
        if (DarkTableUI.RENDER_TYPE_RADIOBUTTON.equals(table.getClientProperty(DarkTableUI.KEY_BOOLEAN_RENDER_TYPE))) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
