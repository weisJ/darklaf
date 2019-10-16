package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;

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
            new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox());
    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorRadioButton> radioRenderer =
            new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorRadioButton());

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

        if (DarkTableCellFocusBorder.isRowFocusBorder(table)
                && table.getSelectionModel().getLeadSelectionIndex() == row
                && !table.isEditing()
                && DarkUIUtil.hasFocus(table)) {
            component.setBorder(UIManager.getBorder("Table.focusSelectedCellHighlightBorder"));
            component.putClientProperty("JTable.rowFocusBorder", true);
            JTableHeader header = table.getTableHeader();
            TableColumn draggedColumn = (header == null) ? null : header.getDraggedColumn();
            boolean forceLeft = false;
            boolean forceRight = false;
            if (draggedColumn != null) {
                int index = DarkTableUI.viewIndexForColumn(draggedColumn, table);
                forceLeft = column == index + 1 || column == index;
                forceRight = column == index - 1 || column == index;
            }
            component.putClientProperty("JTable.forcePaintRight", forceRight);
            component.putClientProperty("JTable.forcePaintLeft", forceLeft);
        } else {
            component.putClientProperty("JTable.rowFocusBorder", false);
        }

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    component.setBackground(alternativeRowColor);
                } else {
                    component.setBackground(normalColor);
                }
                component.setForeground(table.getSelectionForeground());
            }
        }
        return component;
    }

    protected static boolean isBooleanRenderingEnabled(@NotNull final JTable table) {
        return Boolean.TRUE.equals(table.getClientProperty("JTable.renderBooleanAsCheckBox"));
    }

    protected TableCellRenderer getBooleanRenderer(@NotNull final JTable table) {
        if ("radioButton".equals(table.getClientProperty("JTable.booleanRenderType"))) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
