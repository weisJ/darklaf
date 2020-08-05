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
 *
 */
package com.github.weisj.darklaf.ui.table.renderer;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.table.TableCellEditor;

import com.github.weisj.darklaf.delegate.TableCellEditorDelegate;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.github.weisj.darklaf.ui.spinner.DarkSpinnerUI;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TableConstants;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTableCellEditorDelegate extends TableCellEditorDelegate {

    private static final IconWrapper iconWrapper = new IconWrapper();

    private final JToggleButton editorCheckBox = new DarkCellRendererToggleButton.CellCheckBox(true);
    private final DarkTableCellEditorToggleButton checkBoxEditor = new DarkTableCellEditorToggleButton(editorCheckBox);
    private final JToggleButton editorRadioButton = new DarkCellRendererToggleButton.CellRadioButton(true);
    private final DarkTableCellEditorToggleButton radioButtonEditor = new DarkTableCellEditorToggleButton(editorRadioButton);

    private boolean isBooleanEditor;
    private TableCellEditor currentEditor;

    public DarkTableCellEditorDelegate() {
        super(new DarkMultiCellEditor());
    }

    public DarkTableCellEditorDelegate(final TableCellEditor editor) {
        super(editor);
    }

    @Override
    public Object getCellEditorValue() {
        if (isBooleanEditor && currentEditor != null) {
            return currentEditor.getCellEditorValue();
        }
        return super.getCellEditorValue();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        if (anEvent == null) return super.isCellEditable(null);
        JTable table = ((JTable) anEvent.getSource());
        if (anEvent instanceof KeyEvent) {
            if (DarkTableUI.ignoreKeyCodeOnEdit((KeyEvent) anEvent, table)) return false;
        }
        if (anEvent instanceof MouseEvent && TableConstants.isBooleanRenderingEnabled(table)) {
            Point p = ((MouseEvent) anEvent).getPoint();
            int row = table.rowAtPoint(p);
            int col = table.columnAtPoint(p);
            if (row >= 0 && row < table.getRowCount() && col >= 0 && col < table.getColumnCount()) {
                Object value = table.getValueAt(row, col);
                if (TableConstants.useBooleanEditorForValue(value, table, col)) {
                    Rectangle rect = table.getCellRect(row, col, false);
                    p.x -= rect.x;
                    p.y -= rect.y;
                    Component editor = getBooleanEditor(table).getTableCellEditorComponent(table, true,
                                                                                           false, row, col);
                    editor.setBounds(rect);
                    return editor.contains(p);
                }
            }
        }
        return super.isCellEditable(anEvent);
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                                                 final int row, final int column) {
        isBooleanEditor = TableConstants.useBooleanEditorForValue(value, table, column);
        currentEditor = isBooleanEditor ? getBooleanEditor(table) : getDelegate();

        Component editor = currentEditor.getTableCellEditorComponent(table, value, isSelected, row, column);
        editor = prepareEditorComponent(editor, table, value, isSelected, row, column);
        return editor;
    }

    protected Component prepareEditorComponent(final Component editor, final JTable table, final Object value,
                                               final boolean isSelected,
                                               final int row, final int column) {
        return prepareEditor(editor, table, value, isSelected, row, column);
    }

    public static Component prepareEditor(final Component editor, final JTable table, final Object value,
                                          final boolean isSelected,
                                          final int row, final int column) {
        Component renderer = table.getCellRenderer(row, column)
                                  .getTableCellRendererComponent(table, value, isSelected, false, row, column);
        setupEditorComponent(editor, value, renderer);
        Component comp = applyRendererIcon(editor, renderer);
        CellUtil.setupTableBackground(comp, table, false, row);
        return comp;
    }

    protected static void setupEditorComponent(final Component editorComponent, final Object value,
                                               final Component rendererComp) {
        if (editorComponent instanceof JSpinner) {
            int alignment = getHorizontalAlignment(rendererComp);
            if (alignment >= 0) {
                ((JComponent) editorComponent).putClientProperty(DarkSpinnerUI.KEY_EDITOR_ALIGNMENT, alignment);
            }
        }
        if (editorComponent instanceof JTextField) {
            int alignment = getHorizontalAlignment(rendererComp);
            if (alignment >= 0) ((JTextField) editorComponent).setHorizontalAlignment(alignment);
        }
    }

    protected static int getHorizontalAlignment(final Component rendererComp) {
        if (rendererComp instanceof JTextField) {
            return ((JTextField) rendererComp).getHorizontalAlignment();
        } else if (rendererComp instanceof JLabel) {
            return ((JLabel) rendererComp).getHorizontalAlignment();
        }
        return -1;
    }

    protected static Component applyRendererIcon(final Component editorComponent, final Component rendererComponent) {
        Component comp = editorComponent;
        if (rendererComponent instanceof JLabel && comp instanceof JComponent) {
            Icon icon = ((JLabel) rendererComponent).getIcon();
            if (icon != null) {
                iconWrapper.init((JComponent) comp, icon, rendererComponent.getComponentOrientation());
                iconWrapper.setIconGap(((JLabel) rendererComponent).getIconTextGap() - 1);
                comp = iconWrapper;
            }
        }
        return comp;
    }

    protected TableCellEditor getBooleanEditor(final JTable table) {
        if (PropertyUtil.isPropertyEqual(table, DarkTableUI.KEY_BOOLEAN_RENDER_TYPE,
                                         DarkTableUI.RENDER_TYPE_RADIOBUTTON)) {
            return radioButtonEditor;
        }
        return checkBoxEditor;
    }
}
