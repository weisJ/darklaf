package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.combobox.DarkComboBoxUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 */
public class DarkTableCellEditor extends DefaultCellEditor {

    private static final JCheckBox dummyCheckBox = new JCheckBox();

    private final DarkTableCellEditorToggleButton checkBoxEditor =
            new DarkTableCellEditorToggleButton(this, new DarkTableCellEditorToggleButton.CellCheckBox());
    private final DarkTableCellEditorToggleButton radioButtonEditor =
            new DarkTableCellEditorToggleButton(this, new DarkTableCellEditorToggleButton.CellRadioButton());
    private boolean value;
    private boolean isBooleanEditor;

    public DarkTableCellEditor() {
        this(new JTextField());
    }

    public DarkTableCellEditor(final JComboBox<?> comboBox) {
        super(comboBox);
    }

    public DarkTableCellEditor(@NotNull final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        spinner.putClientProperty("JSpinner.isTableCellEditor", Boolean.TRUE);
        setClickCountToStart(2);
        delegate = new EditorDelegate() {
            public void setValue(final Object value) {
                try {
                    spinner.setValue(NumberFormat.getInstance().parse(value.toString()));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }

            public Object getCellEditorValue() {
                return spinner.getValue();
            }

            public boolean shouldSelectCell(final EventObject anEvent) {
                if (anEvent instanceof MouseEvent) {
                    MouseEvent e = (MouseEvent) anEvent;
                    return e.getID() != MouseEvent.MOUSE_DRAGGED;
                }
                return true;
            }
        };
    }

    public DarkTableCellEditor(final JCheckBox checkBox) {
        this((JToggleButton) checkBox);
    }

    public DarkTableCellEditor(final JRadioButton checkBox) {
        this((JToggleButton) checkBox);
    }

    public DarkTableCellEditor(@NotNull final JToggleButton toggleButton) {
        super(dummyCheckBox);
        editorComponent = toggleButton;
        delegate = new EditorDelegate() {
            public void setValue(final Object value) {
                boolean selected = false;
                if (value instanceof Boolean) {
                    selected = (Boolean) value;
                } else if (value instanceof String) {
                    selected = value.equals("true");
                }
                toggleButton.setSelected(selected);
            }

            public Object getCellEditorValue() {
                return toggleButton.isSelected();
            }
        };
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
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

    @SuppressWarnings("unchecked")
    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {

        if (useBooleanEditor(value, table)) {
            isBooleanEditor = true;
            return getBooleanEditor(table).getTableCellEditorComponent(table, value, isSelected, row, column);
        } else {
            isBooleanEditor = false;
        }

        delegate.setValue(value);

        if (editorComponent instanceof JComboBox) {
            ((JComboBox<?>) editorComponent).removeAllItems();
            ((JComboBox<Object>) editorComponent).addItem(value);
            ((JComboBox<?>) editorComponent).setSelectedItem(value);
        } else if (editorComponent instanceof JSpinner) {
            var rendererComp = table.getCellRenderer(row, column)
                                    .getTableCellRendererComponent(table, value, isSelected, false, row, column);
            if (rendererComp instanceof JTextField) {
                editorComponent.putClientProperty("JSpinner.cellEditorAlignment",
                                                  ((JTextField) rendererComp).getHorizontalAlignment());
            } else if (rendererComp instanceof JLabel) {
                editorComponent.putClientProperty("JSpinner.cellEditorAlignment",
                                                  ((JLabel) rendererComp).getHorizontalAlignment());
            }
        }

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

    @Contract("null, _ -> false")
    private boolean useBooleanEditor(final Object value, final JTable table) {
        return value instanceof Boolean && DarkTableCellRenderer.isBooleanRenderingEnabled(table)
               && !(editorComponent instanceof JCheckBox || editorComponent instanceof JRadioButton);
    }

    protected TableCellEditor getBooleanEditor(@NotNull final JTable table) {
        if ("radioButton".equals(table.getClientProperty("JTable.booleanRenderType"))) {
            return radioButtonEditor;
        }
        return checkBoxEditor;
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
    public boolean stopCellEditing() {
        if (editorComponent instanceof JComboBox) {
            ((DarkComboBoxUI) ((JComboBox<?>) editorComponent).getUI()).resetPopup();
        }
        return super.stopCellEditing();
    }

    @Override
    public boolean isCellEditable(@NotNull final EventObject anEvent) {
        var table = ((JTable) anEvent.getSource());
        if (DarkTableCellRenderer.isBooleanRenderingEnabled(table)) {
            var p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, table);
            int row = table.rowAtPoint(p);
            int col = table.columnAtPoint(p);
            if (row >= 0 && row < table.getRowCount() && col >= 0 && col < table.getColumnCount()) {
                var value = table.getValueAt(row, col);
                if (useBooleanEditor(value, table)) {
                    return true;
                }
            }
        }
        return super.isCellEditable(anEvent);
    }
}
