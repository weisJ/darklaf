/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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

import com.github.weisj.darklaf.decorators.CellRenderer;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.util.EventObject;

/**
 * @author vincencopalazzo
 * @author atarw
 * @author Jannis Weis
 */
public class DarkTableCellEditorToggleButton extends AbstractCellEditor implements TableCellEditor, SwingConstants {

    private final JToggleButton toggleButton;

    public DarkTableCellEditorToggleButton(final DarkTableCellEditor delegate,
                                           final JToggleButton toggleButton) {
        this.toggleButton = toggleButton;
        toggleButton.setOpaque(true);
        toggleButton.addChangeListener(e -> delegate.setValue(toggleButton.isSelected()));
    }

    @Override
    public Component getTableCellEditorComponent(final JTable table, final Object value,
                                                 final boolean isSelected, final int row, final int column) {
        if (value instanceof Boolean) {
            toggleButton.setSelected((Boolean) value);
        }
        toggleButton.setHorizontalAlignment(table.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);

        boolean alternativeRow = Boolean.TRUE.equals(table.getClientProperty("JTable.alternateRowColor"));
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = table.getBackground();
        var background = alternativeRow && row % 2 == 1 ? alternativeRowColor : normalColor;
        if (!(isSelected) || table.isEditing()) {
            toggleButton.setBackground(background);
            toggleButton.setForeground(table.getForeground());
        } else {
            if (DarkUIUtil.hasFocus(table)) {
                toggleButton.setForeground(table.getSelectionForeground());
            } else {
                toggleButton.setForeground(UIManager.getColor("Table.selectionForegroundInactive"));
            }
            toggleButton.setBackground(table.getSelectionBackground());
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

    public static class CellCheckBox extends JCheckBox implements CellRenderer {
        @Override
        public boolean hasFocus() {
            return true;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }

    public static class CellRadioButton extends JRadioButton implements CellRenderer {
        @Override
        public boolean hasFocus() {
            return true;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }
}
