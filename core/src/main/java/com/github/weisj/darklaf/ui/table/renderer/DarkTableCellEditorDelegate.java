/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
import com.github.weisj.darklaf.ui.spinner.DarkSpinnerUI;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TableConstants;

public class DarkTableCellEditorDelegate extends TableCellEditorDelegate {

    private static final IconWrapper iconWrapper = new IconWrapper();

    public DarkTableCellEditorDelegate() {
        super(new DarkMultiCellEditor());
    }

    public DarkTableCellEditorDelegate(final TableCellEditor editor) {
        super(editor);
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        if (anEvent == null) return super.isCellEditable(null);
        JTable table = ((JTable) anEvent.getSource());
        if (anEvent instanceof KeyEvent) {
            if (DarkTableUI.ignoreKeyCodeOnEdit((KeyEvent) anEvent, table)) return false;
        }
        if (TableConstants.isBooleanRenderingEnabled(table)) {
            if (anEvent instanceof MouseEvent) {
                Point p = ((MouseEvent) anEvent).getPoint();
                int row = table.rowAtPoint(p);
                int col = table.columnAtPoint(p);
                if (isValidIndex(table, row, col)) {
                    Object value = table.getValueAt(row, col);
                    if (TableConstants.useBooleanEditorForValue(value, table, col, false)) {
                        return insideBooleanRenderer(table, row, col, p);
                    }
                }
            }
        }
        return super.isCellEditable(anEvent);
    }

    private boolean isValidIndex(final JTable table, final int row, final int col) {
        return row >= 0 && row < table.getRowCount() && col >= 0 && col < table.getColumnCount();
    }

    private boolean insideBooleanRenderer(final JTable table, final int row, final int col, final Point p) {
        Rectangle rect = table.getCellRect(row, col, false);
        p.x -= rect.x;
        p.y -= rect.y;
        Component editor =
                table.getCellRenderer(row, col).getTableCellRendererComponent(table, true, false, false, row, col);
        editor.setBounds(rect);
        return editor.contains(p);
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
            final int row, final int column) {
        if (TableConstants.useBooleanEditorForValue(value, table, column)) {
            table.setValueAt(!(boolean) value, row, column);
            table.repaint();
            // returning null will stop cell editing immediately.
            return null;
        }
        Component editor = getDelegate().getTableCellEditorComponent(table, value, isSelected, row, column);
        editor = prepareEditorComponent(editor, table, value, isSelected, row, column);
        return editor;
    }

    protected Component prepareEditorComponent(final Component editor, final JTable table, final Object value,
            final boolean isSelected, final int row, final int column) {
        return prepareEditor(editor, table, value, isSelected, row, column);
    }

    public static Component prepareEditor(final Component editor, final JTable table, final Object value,
            final boolean isSelected, final int row, final int column) {
        Component renderer = table.getCellRenderer(row, column).getTableCellRendererComponent(table, value, isSelected,
                false, row, column);
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
}
