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
package com.github.weisj.darklaf.delegate;

import java.awt.*;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.event.CellEditorListener;
import javax.swing.table.TableCellEditor;

public class TableCellEditorDelegate implements TableCellEditor {

    private final TableCellEditor editor;

    public TableCellEditorDelegate(final TableCellEditor editor) {
        this.editor = editor;
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                                                 final int row, final int column) {
        return editor.getTableCellEditorComponent(table, value, isSelected, row, column);
    }

    @Override
    public Object getCellEditorValue() {
        return editor.getCellEditorValue();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        return editor.isCellEditable(anEvent);
    }

    @Override
    public boolean shouldSelectCell(final EventObject anEvent) {
        return editor.shouldSelectCell(anEvent);
    }

    @Override
    public boolean stopCellEditing() {
        return editor.stopCellEditing();
    }

    @Override
    public void cancelCellEditing() {
        editor.cancelCellEditing();
    }

    @Override
    public void addCellEditorListener(final CellEditorListener l) {
        editor.addCellEditorListener(l);
    }

    @Override
    public void removeCellEditorListener(final CellEditorListener l) {
        editor.removeCellEditorListener(l);
    }
}
