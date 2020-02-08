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

import com.github.weisj.darklaf.ui.combobox.DarkComboBoxUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellEditor extends DefaultCellEditor {

    private static final JCheckBox dummyCheckBox = new JCheckBox();
    private static final IconWrapper iconWrapper = new IconWrapper();

    private final DarkTableCellEditorToggleButton checkBoxEditor =
            new DarkTableCellEditorToggleButton(this, new DarkTableCellEditorToggleButton.CellCheckBox());
    private final DarkTableCellEditorToggleButton radioButtonEditor =
            new DarkTableCellEditorToggleButton(this, new DarkTableCellEditorToggleButton.CellRadioButton());
    private boolean value;
    private boolean isBooleanEditor;
    private JTable table;

    public DarkTableCellEditor() {
        this(new JTextField());
    }

    public DarkTableCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(new TextFieldTableCellEditorBorder());
        textField.putClientProperty("JTextField.isCellEditor", Boolean.TRUE);
        setClickCountToStart(2);
    }

    public DarkTableCellEditor(final JComboBox<?> comboBox) {
        super(comboBox);
        comboBox.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(final PopupMenuEvent e) {
                if (table != null) table.repaint();
            }

            @Override
            public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
                if (table != null) table.repaint();
            }

            @Override
            public void popupMenuCanceled(final PopupMenuEvent e) {

            }
        });
        setClickCountToStart(2);
    }

    public DarkTableCellEditor(@NotNull final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        spinner.putClientProperty("JSpinner.isTableCellEditor", Boolean.TRUE);
        setClickCountToStart(2);
        delegate = new EditorDelegate() {
            public Object getCellEditorValue() {
                return spinner.getValue();
            }

            public void setValue(final Object value) {
                try {
                    SpinnerModel model = spinner.getModel();
                    if (model instanceof SpinnerNumberModel) {
                        spinner.setValue(NumberFormat.getInstance().parse(value.toString()));
                    } else if (model instanceof SpinnerDateModel) {
                        spinner.setValue(DateFormat.getInstance().parse(value.toString()));
                    } else {
                        spinner.setValue(value);
                    }
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }

            public boolean shouldSelectCell(final EventObject anEvent) {
                if (anEvent instanceof MouseEvent) {
                    MouseEvent e = (MouseEvent) anEvent;
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

    public DarkTableCellEditor(@NotNull final JToggleButton toggleButton) {
        super(dummyCheckBox);
        editorComponent = toggleButton;
        delegate = new EditorDelegate() {
            public Object getCellEditorValue() {
                return toggleButton.isSelected();
            }

            public void setValue(final Object value) {
                boolean selected = false;
                if (value instanceof Boolean) {
                    selected = (Boolean) value;
                } else if (value instanceof String) {
                    selected = value.equals("true");
                }
                toggleButton.setSelected(selected);
            }
        };
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
    }

    public DarkTableCellEditor(final JRadioButton checkBox) {
        this((JToggleButton) checkBox);
    }

    public void setValue(final Object value) {
        delegate.setValue(value);
        if (value instanceof Boolean) {
            this.value = (boolean) value;
        } else {
            isBooleanEditor = false;
        }
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
    public boolean isCellEditable(final EventObject anEvent) {
        if (anEvent == null) return super.isCellEditable(anEvent);
        JTable table = ((JTable) anEvent.getSource());
        if (DarkTableCellRenderer.isBooleanRenderingEnabled(table) && anEvent instanceof MouseEvent) {
            Point p = ((MouseEvent) anEvent).getPoint();
            int row = table.rowAtPoint(p);
            int col = table.columnAtPoint(p);
            if (row >= 0 && row < table.getRowCount() && col >= 0 && col < table.getColumnCount()) {
                Object value = table.getValueAt(row, col);
                if (useBooleanEditor(value, table)) {
                    Rectangle rect = table.getCellRect(row, col, false);
                    p.x -= rect.x;
                    p.y -= rect.y;
                    Component editor = getBooleanEditor(table).getTableCellEditorComponent(table, true,
                                                                                           false, row, col);
                    return editor.contains(p);
                }
            }
        }
        return super.isCellEditable(anEvent);
    }

    @Override
    public boolean stopCellEditing() {
        if (editorComponent instanceof JComboBox) {
            ((DarkComboBoxUI) ((JComboBox<?>) editorComponent).getUI()).resetPopup();
        }
        return super.stopCellEditing();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        this.table = table;
        if (useBooleanEditor(value, table)) {
            isBooleanEditor = true;
            return getBooleanEditor(table).getTableCellEditorComponent(table, value, isSelected, row, column);
        } else {
            isBooleanEditor = false;
        }

        delegate.setValue(value);

        JComponent comp = editorComponent;

        if (editorComponent instanceof JComboBox) {
            ((JComboBox<?>) editorComponent).removeAllItems();
            ((JComboBox<Object>) editorComponent).addItem(value);
            ((JComboBox<?>) editorComponent).setSelectedItem(value);
        } else if (editorComponent instanceof JSpinner) {
            Component rendererComp = table.getCellRenderer(row, column)
                                          .getTableCellRendererComponent(table, value, isSelected, false, row, column);
            if (rendererComp instanceof JTextField) {
                editorComponent.putClientProperty("JSpinner.cellEditorAlignment",
                                                  ((JTextField) rendererComp).getHorizontalAlignment());
            } else if (rendererComp instanceof JLabel) {
                editorComponent.putClientProperty("JSpinner.cellEditorAlignment",
                                                  ((JLabel) rendererComp).getHorizontalAlignment());
            }
        }

        boolean alternativeRow = Boolean.TRUE.equals(table.getClientProperty("JTable.alternateRowColor"));
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = table.getBackground();
        Color background = alternativeRow && row % 2 == 1 ? alternativeRowColor : normalColor;

        Component rendererComp = table.getCellRenderer(row, column)
                                .getTableCellRendererComponent(table, value, isSelected, false, row, column);
        if (rendererComp instanceof JLabel) {
            Icon icon = ((JLabel) rendererComp).getIcon();
            if (icon != null) {
                comp = iconWrapper;
                iconWrapper.setBackground(background);
                iconWrapper.init(editorComponent, icon, rendererComp.getComponentOrientation().isLeftToRight());
                iconWrapper.setIconGap(((JLabel) rendererComp).getIconTextGap() - 1);
            }
        }

        if (!(isSelected)) {
            comp.setBackground(background);
        }
        return comp;
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

    protected static class IconWrapper extends JPanel {

        private final JLabel label;
        private JComponent c;
        private int iconGap;

        protected IconWrapper() {
            setLayout(null);
            label = new JLabel();
            label.setIconTextGap(0);
            add(label);
        }

        protected void setIconGap(final int iconGap) {
            this.iconGap = iconGap;
        }

        protected void init(@NotNull final JComponent component, final Icon icon, final boolean ltr) {
            setComponentOrientation(ltr ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT);
            if (c != null) {
                remove(c);
            }
            add(component);
            this.c = component;
            label.setIcon(icon);
        }

        @Override
        public void doLayout() {
            if (c == null) return;
            int w = getWidth();
            int h = getHeight();
            Border b = c.getBorder();
            Insets ins = new Insets(0, 0, 0, 0);
            Dimension labelSize = label.getPreferredSize();
            int gap = getIconCompGap();
            if (b != null) {
                ins = b.getBorderInsets(c);
            }
            if (getComponentOrientation().isLeftToRight()) {
                label.setBounds(ins.left + gap, 0, labelSize.width + 1, h);
                c.setBounds(ins.left + labelSize.width + 2 * gap - 1, 0,
                            w - ins.left - labelSize.width - 2 * gap + 1, h);
            } else {
                c.setBounds(0, 0, w - ins.right - labelSize.width - gap - 1, h);
                label.setBounds(w - ins.right - labelSize.width - gap - 1, 0, labelSize.width + 1, h);
            }
        }

        public int getIconCompGap() {
            return iconGap;
        }
    }
}
