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
import java.util.EventObject;

import javax.swing.*;
import javax.swing.table.TableCellEditor;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellEditorToggleButton extends AbstractCellEditor implements TableCellEditor, SwingConstants {

    private final JToggleButton toggleButton;

    public DarkTableCellEditorToggleButton(final JToggleButton toggleButton) {
        this.toggleButton = toggleButton;
        toggleButton.setOpaque(true);
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        if (value instanceof Boolean) {
            toggleButton.setSelected((Boolean) value);
        }
        return toggleButton;
    }

    @Override
    public Object getCellEditorValue() {
        return toggleButton.isSelected();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        return true;
    }

    @Override
    public boolean shouldSelectCell(final EventObject anEvent) {
        return false;
    }
}
