package com.weis.darklaf.ui.table;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

/**
 * @author vincencopalazzo
 * @author atarw
 */
public class DarkTableCellRenderer extends DefaultTableCellRenderer {

    private final DarkTableCellRendererCheckBox checkBoxRenderer = new DarkTableCellRendererCheckBox();
    private final DarkTableCellRendererRadioButton radioRenderer = new DarkTableCellRendererRadioButton();

    protected static boolean isBooleanRenderingEnabled(@NotNull final JTable table) {
        return Boolean.TRUE.equals(table.getClientProperty("JTable.renderBooleanAsCheckBox"));
    }

    protected TableCellRenderer getBooleanRenderer(@NotNull final JTable table) {
        if ("radioButton".equals(table.getClientProperty("JTable.booleanRenderType"))) {
            return radioRenderer;
        }
        return checkBoxRenderer;
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

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    component.setBackground(alternativeRowColor);
                    setDefaultCellRenderWithAllType(table, value, false, hasFocus, row, column,
                                                    alternativeRowColor);
                } else {
                    component.setBackground(normalColor);
                    setDefaultCellRenderWithAllType(table, value, false, hasFocus, row, column, normalColor);
                }
                component.setForeground(table.getSelectionForeground());
            }
        }
        return component;
    }

    // This method setting a MaterialCellRender at the particular class
    // With this class not working correctly the color alternate in the Jtable
    // in particular the IconImage without this code the cell is painted not correctly or
    // in the cell did print the path of the image
    protected void setDefaultCellRenderWithAllType(final JTable table, final Object value, final boolean isSelected,
                                                   final boolean hasFocus, final int row, final int column,
                                                   final Color color) {
        if (table == null) {
            throw new IllegalArgumentException("Table is null");
        }

        Component component = table.getDefaultRenderer(ImageIcon.class)
                                   .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        component.setBackground(color);
    }
}
