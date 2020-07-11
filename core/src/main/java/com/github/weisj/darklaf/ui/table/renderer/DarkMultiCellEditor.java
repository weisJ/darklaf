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
import java.util.Date;

import javax.swing.*;
import javax.swing.table.TableCellEditor;

import com.github.weisj.darklaf.delegate.TableCellEditorDelegate;
import com.github.weisj.darklaf.util.LazyValue;

public class DarkMultiCellEditor extends TableCellEditorDelegate {

    private final LazyValue<DarkTableCellEditor> numberEditor = new LazyValue<>(() -> new DarkTableCellEditor(new JSpinner()));
    private final LazyValue<DarkTableCellEditor> dateEditor = new LazyValue<>(() -> new DarkTableCellEditor(new JSpinner(new SpinnerDateModel())));
    private TableCellEditor currentEditor;

    public DarkMultiCellEditor() {
        super(new DarkTableCellEditor());
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                                                 final int row, final int column) {
        currentEditor = getEditor(table, value, column);
        return super.getTableCellEditorComponent(table, value, isSelected, row, column);
    }

    private TableCellEditor getEditor(final JTable table, final Object value, final int column) {
        Class<?> columnClass = table.getColumnClass(column);
        if (columnClass != null && columnClass.isInstance(value)) {
            if (Number.class.isAssignableFrom(columnClass)) {
                return numberEditor.get();
            } else if (Date.class.isAssignableFrom(columnClass)) {
                return dateEditor.get();
            }
        }
        return null;
    }

    @Override
    public TableCellEditor getDelegate() {
        return currentEditor != null ? currentEditor : super.getDelegate();
    }
}
