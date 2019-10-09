package com.weis.darklaf.ui.table;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.EventObject;

/**
 * @author Jannis Weis
 */
public class DarkColorTableCellRendererEditor extends AbstractCellEditor implements TableCellRenderer, TableCellEditor {

    private final JLabel label;
    private Color savedColor;

    public DarkColorTableCellRendererEditor() {
        label = new JLabel();
        label.setOpaque(true);

    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        if (!(value instanceof Color)) {
            cancelCellEditing();
            return table.getCellRenderer(row, column).getTableCellRendererComponent(table, value, isSelected,
                                                                                    true, row, column);
        } else {
            changeColor((Color) value);
            SwingUtilities.invokeLater(() -> {
                Color color = JColorChooser.showDialog(table, "Color Chooser", savedColor);
                if (color == null) {
                    cancelCellEditing();
                } else {
                    changeColor(color);
                }
            });
            return label;
        }
    }

    private void changeColor(final Color color) {
        if (color != null) {
            savedColor = color;
            label.setBackground(color);
        }
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        if (anEvent instanceof MouseEvent) {
            return ((MouseEvent) anEvent).getClickCount() >= 2;
        }
        return true;
    }

    @Override
    public Object getCellEditorValue() {
        return savedColor;
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value,
                                                   final boolean isSelected, final boolean hasFocus,
                                                   final int row, final int column) {
        changeColor((Color) value);
        return label;
    }
}
