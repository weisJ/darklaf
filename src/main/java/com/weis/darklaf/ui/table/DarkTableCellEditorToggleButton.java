package com.weis.darklaf.ui.table;

import com.weis.darklaf.decorators.CellRenderer;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellEditorToggleButton extends AbstractCellEditor implements TableCellEditor, SwingConstants {

    private final JToggleButton toggleButton;

    public DarkTableCellEditorToggleButton(final DarkTableCellEditor delegate,
                                           final JToggleButton toggleButton) {
        this.toggleButton = toggleButton;
        toggleButton.addChangeListener(e -> delegate.setValue(toggleButton.isSelected()));
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        if (value instanceof Boolean) {
            toggleButton.setSelected((Boolean) value);
        }
        toggleButton.setHorizontalAlignment(table.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    toggleButton.setBackground(alternativeRowColor);
                } else {
                    toggleButton.setBackground(normalColor);
                }
                toggleButton.setForeground(table.getForeground());
            } else {
                toggleButton.setForeground(table.getSelectionForeground());
                toggleButton.setBackground(table.getSelectionBackground());
            }
        }
        return toggleButton;
    }

    @Override
    public Object getCellEditorValue() {
        return toggleButton.isSelected();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        return true;
    }

    @Override
    public boolean shouldSelectCell(final EventObject anEvent) {
        return false;
    }

    public static class CellCheckBox extends JCheckBox implements CellRenderer {
        @Override
        public boolean hasFocus() {
            return true;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }

    public static class CellRadioButton extends JRadioButton implements CellRenderer {
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
