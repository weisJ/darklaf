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
        return false;
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
