package com.weis.darklaf.ui.table;

import com.weis.darklaf.decorators.CellRenderer;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 */
public class DarkTableCellEditorCheckBox extends AbstractCellEditor implements TableCellEditor, SwingConstants {

    private final JCheckBox checkBox;

    public DarkTableCellEditorCheckBox(final DarkTableCellEditor delegate) {
        checkBox = new CellEditorCheckBox();
        checkBox.addChangeListener(e -> delegate.setValue(checkBox.isSelected()));
    }


    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        if (value instanceof Boolean) {
            checkBox.setSelected((Boolean) value);
        }
        checkBox.setHorizontalAlignment(table.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    checkBox.setBackground(alternativeRowColor);
                } else {
                    checkBox.setBackground(normalColor);
                }
                checkBox.setForeground(table.getForeground());
            } else {
                checkBox.setForeground(table.getSelectionForeground());
                checkBox.setBackground(table.getSelectionBackground());
            }
        }
        return checkBox;
    }

    @Override
    public Object getCellEditorValue() {
        return checkBox.isSelected();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        return true;
    }

    @Override
    public boolean shouldSelectCell(final EventObject anEvent) {
        return false;
    }

    private static class CellEditorCheckBox extends JCheckBox implements CellRenderer {
        @Override
        public boolean hasFocus() {
            return true;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }
}
