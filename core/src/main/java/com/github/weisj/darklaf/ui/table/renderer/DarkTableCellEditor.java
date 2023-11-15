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
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.text.DefaultFormatter;

import com.github.weisj.darklaf.ui.combobox.ComboBoxConstants;
import com.github.weisj.darklaf.ui.combobox.DarkComboBoxUI;
import com.github.weisj.darklaf.ui.spinner.SpinnerConstants;
import com.github.weisj.darklaf.ui.table.TextTableCellEditorBorder;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;
import com.github.weisj.darklaf.util.PropertyValue;

/** @author Jannis Weis */
public class DarkTableCellEditor extends DefaultCellEditor {

    private static final JCheckBox dummyCheckBox = new JCheckBox();

    public DarkTableCellEditor() {
        this(new JTextField());
    }

    public DarkTableCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(new TextTableCellEditorBorder());
        textField.putClientProperty(DarkTextUI.KEY_IS_TABLE_EDITOR, true);
        setClickCountToStart(2);
    }

    public DarkTableCellEditor(final JComboBox<Object> comboBox) {
        super(comboBox);
        EditorDelegate del = delegate;
        delegate = new EditorDelegate() {
            @Override
            public void setValue(final Object value) {
                comboBox.removeAllItems();
                if (value != null && value.getClass().isArray()) {
                    for (Object obj : (Object[]) value) {
                        comboBox.addItem(obj);
                    }
                } else {
                    comboBox.addItem(value);
                    comboBox.setSelectedItem(value);
                }
            }

            @Override
            public Object getCellEditorValue() {
                return del.getCellEditorValue();
            }

            @Override
            public boolean shouldSelectCell(final EventObject anEvent) {
                return del.shouldSelectCell(anEvent);
            }

            @Override
            public boolean stopCellEditing() {
                return del.stopCellEditing();
            }
        };
        comboBox.putClientProperty(ComboBoxConstants.KEY_IS_TABLE_EDITOR, true);
        setClickCountToStart(2);
    }

    public DarkTableCellEditor(final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        spinner.putClientProperty(SpinnerConstants.KEY_IS_TABLE_EDITOR, Boolean.TRUE);
        setClickCountToStart(2);
        delegate = new EditorDelegate() {
            @Override
            public Object getCellEditorValue() {
                return spinner.getValue();
            }

            @Override
            public void setValue(final Object value) {
                try {
                    SpinnerModel model = spinner.getModel();
                    if (model instanceof SpinnerNumberModel && !(value instanceof Number)) {
                        spinner.setValue(NumberFormat.getInstance().parse(value.toString()));
                    } else if (model instanceof SpinnerDateModel && !(value instanceof Date)) {
                        spinner.setValue(DateFormat.getInstance().parse(value.toString()));
                    } else {
                        spinner.setValue(value);
                    }
                    if (spinner.getValue() != null) {
                        Component editor = spinner.getEditor();
                        if (editor instanceof JSpinner.DefaultEditor defaultEditor) {
                            JFormattedTextField.AbstractFormatter formatter =
                                    defaultEditor.getTextField().getFormatter();
                            if (formatter instanceof DefaultFormatter) {
                                ((DefaultFormatter) formatter).setValueClass(spinner.getValue().getClass());
                            }
                        }
                    }
                } catch (final ParseException e) {
                    throw new IllegalStateException(e);
                }
            }

            @Override
            public boolean shouldSelectCell(final EventObject anEvent) {
                if (anEvent instanceof MouseEvent e) {
                    return e.getID() != MouseEvent.MOUSE_DRAGGED;
                }
                return true;
            }
        };
        setClickCountToStart(2);
    }

    public DarkTableCellEditor(final JCheckBox checkBox) {
        this((JToggleButton) checkBox);
    }

    public DarkTableCellEditor(final JToggleButton toggleButton) {
        super(dummyCheckBox);
        editorComponent = toggleButton;
        delegate = new EditorDelegate() {
            @Override
            public Object getCellEditorValue() {
                return toggleButton.isSelected();
            }

            @Override
            public void setValue(final Object value) {
                boolean selected = false;
                if (value instanceof Boolean) {
                    selected = (Boolean) value;
                } else if (value instanceof String) {
                    selected = value.equals(PropertyValue.TRUE);
                }
                toggleButton.setSelected(selected);
            }
        };
        toggleButton.putClientProperty(ToggleButtonConstants.KEY_IS_TABLE_EDITOR, true);
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
    }

    @Override
    public boolean stopCellEditing() {
        if (editorComponent instanceof JComboBox) {
            ((DarkComboBoxUI) ((JComboBox<?>) editorComponent).getUI()).resetPopup();
        }
        return super.stopCellEditing();
    }
}
