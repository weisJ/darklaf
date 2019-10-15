package com.weis.darklaf.ui.table;

import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.ui.combobox.DarkComboBoxUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
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
                    var model = spinner.getModel();
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
        var table = ((JTable) anEvent.getSource());
        if (DarkTableCellRenderer.isBooleanRenderingEnabled(table) && anEvent instanceof MouseEvent) {
            var p = ((MouseEvent) anEvent).getPoint();
            int row = table.rowAtPoint(p);
            int col = table.columnAtPoint(p);
            if (row >= 0 && row < table.getRowCount() && col >= 0 && col < table.getColumnCount()) {
                var value = table.getValueAt(row, col);
                if (useBooleanEditor(value, table)) {
                    var rect = table.getCellRect(row, col, false);
                    p.x -= rect.x;
                    p.y -= rect.y;
                    var editor = getBooleanEditor(table).getTableCellEditorComponent(table, true,
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

        var comp = editorComponent;

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

        var rendererComp = table.getCellRenderer(row, column)
                                .getTableCellRendererComponent(table, value, isSelected, false, row, column);
        if (rendererComp instanceof JLabel) {
            var icon = ((JLabel) rendererComp).getIcon();
            if (icon != null) {
                comp = iconWrapper;
                iconWrapper.init(editorComponent, icon, rendererComp.getComponentOrientation().isLeftToRight());
                iconWrapper.setIconGap(((JLabel) rendererComp).getIconTextGap() - 1);
            }
        }

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = DarkColors.get().getTableAlternativeBackground();
        Color normalColor = DarkColors.get().getTableBackground();
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    comp.setBackground(alternativeRowColor);
                } else {
                    comp.setBackground(normalColor);
                }
            }
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
            var b = c.getBorder();
            var ins = new Insets(0, 0, 0, 0);
            var labelSize = label.getPreferredSize();
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
