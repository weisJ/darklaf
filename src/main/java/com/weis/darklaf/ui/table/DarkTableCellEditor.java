package com.weis.darklaf.ui.table;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 */
public class DarkTableCellEditor extends DefaultCellEditor {

    private final DarkTableCellEditorCheckBox booleanEditor = new DarkTableCellEditorCheckBox(this);
    private boolean value;
    private boolean isBooleanEditor;

    public DarkTableCellEditor() {
        this(new JTextField());
    }

    public DarkTableCellEditor(final JComboBox<?> comboBox) {
        super(comboBox);
    }

    public DarkTableCellEditor(final JCheckBox checkBox) {
        super(checkBox);
    }

    public DarkTableCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(new DarkTableCellBorder());
    }

    protected void setValue(final Object value) {
        delegate.setValue(value);
        if (value instanceof Boolean) {
            this.value = (boolean) value;
        } else {
            isBooleanEditor = false;
        }
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {

        if (value instanceof Boolean && DarkTableCellRenderer.isBooleanRenderingEnabled(table)) {
            isBooleanEditor = true;
            return booleanEditor.getTableCellEditorComponent(table, value, isSelected, row, column);
        } else {
            isBooleanEditor = false;
        }

        super.getTableCellEditorComponent(table, value, isSelected, row, column);
        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    editorComponent.setBackground(alternativeRowColor);
                } else {
                    editorComponent.setBackground(normalColor);
                }
            }
        }
        return editorComponent;
    }

    @Override
    public Object getCellEditorValue() {
        if (isBooleanEditor) {
            return value;
        } else {
            return super.getCellEditorValue();
        }
    }

    @Override
    public boolean isCellEditable(@NotNull final EventObject anEvent) {
        var table = ((JTable) anEvent.getSource());
        if (DarkTableCellRenderer.isBooleanRenderingEnabled(table)) {
            var p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, table);
            var value = table.getValueAt(table.rowAtPoint(p), table.columnAtPoint(p));
            if (value instanceof Boolean) return true;
        }
        return super.isCellEditable(anEvent);
    }
}
