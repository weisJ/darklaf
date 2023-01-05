/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.table.renderer;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.github.weisj.darklaf.delegate.TableCellRendererDelegate;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.table.DarkTableCellFocusBorder;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TableConstants;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTableCellRendererDelegate extends TableCellRendererDelegate implements TableConstants {

    private final TableCellRenderer booleanCellRenderer;

    public TableCellRenderer getBooleanCellRenderer() {
        return booleanCellRenderer;
    }

    public DarkTableCellRendererDelegate(final TableCellRenderer renderer,
            final TableCellRenderer booleanCellRenderer) {
        super(renderer);
        this.booleanCellRenderer = booleanCellRenderer;
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected,
            final boolean hasFocus, final int row, final int column) {
        boolean booleanRenderer = useBooleanRenderer(table, value, column);
        boolean isRowFocus = DarkTableCellFocusBorder.isRowFocusBorder(table)
                && table.getSelectionModel().getLeadSelectionIndex() == row;
        boolean rowLeadSelection = table.getSelectionModel().getLeadSelectionIndex() == row;
        boolean columnLeadSelection = table.getColumnModel().getSelectionModel().getLeadSelectionIndex() == column;
        if (rowLeadSelection && !columnLeadSelection
                && PropertyUtil.getBooleanProperty(table, TableConstants.KEY_FULL_ROW_FOCUS_BORDER)) {
            columnLeadSelection = true;
        }
        boolean isLeadSelectionCell = DarkUIUtil.hasFocus(table) && rowLeadSelection && columnLeadSelection;

        Component delegateComp = getDelegate()
                .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        Component component;
        if (booleanRenderer) {
            Component booleanComp = getBooleanRenderer()
                    .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            booleanComp.setForeground(delegateComp.getForeground());
            booleanComp.setBackground(delegateComp.getBackground());
            booleanComp.setFont(delegateComp.getFont());
            component = booleanComp;
        } else {
            component = delegateComp;
        }

        if (component instanceof JComponent) {
            setupBorderStyle(table, column, (JComponent) component, isLeadSelectionCell, isRowFocus, isSelected);
        }
        CellUtil.setupTableForeground(component, table, isSelected, row);
        CellUtil.setupTableBackground(component, table, isSelected, row);
        return component;
    }

    public TableCellRenderer getBooleanRenderer() {
        return booleanCellRenderer;
    }

    protected boolean useBooleanRenderer(final JTable table, final Object value, final int column) {
        return TableConstants.useBooleanEditorForValue(value, table, column);
    }

    public void setupBorderStyle(final JTable table, final int column, final JComponent component,
            final boolean isLeadSelectionCell, final boolean isRowFocus, final boolean isSelected) {
        Border focusBorder = UIManager.getBorder("Table.focusSelectedCellHighlightBorder");
        boolean belongsToLeadSelection = isRowFocus || isLeadSelectionCell;
        boolean showLeadFocusBorder = !table.isEditing()
                && belongsToLeadSelection
                && !isSelected;
        if (showLeadFocusBorder) {
            PropertyUtil.installBorder(component, focusBorder);
            if (isRowFocus) {
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
        } else if (component.getBorder() == focusBorder
                || focusBorder.getClass().isInstance(component.getBorder())) {
            component.setBorder(UIManager.getBorder("Table.cellNoFocusBorder"));
        }
    }
}
