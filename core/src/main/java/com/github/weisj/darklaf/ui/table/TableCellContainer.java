/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.table;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.cell.hint.AbstractIndexedCellContainer;
import com.github.weisj.darklaf.util.Pair;

public class TableCellContainer extends AbstractIndexedCellContainer<JTable, Pair<Integer, Integer>, DarkTableUI> {

    private final JTable table;

    public TableCellContainer(final JTable table, final DarkTableUI ui) {
        super(ui);
        this.table = table;
    }

    @Override
    public Rectangle getCellBoundsAt(final Pair<Integer, Integer> position, final boolean isEditing) {
        return isEditing ? table.getEditorComponent().getBounds()
                : table.getCellRect(position.getFirst(), position.getSecond(), false);
    }

    @Override
    public Pair<Integer, Integer> getCellPosition(final Point p) {
        return new Pair<>(table.rowAtPoint(p), table.columnAtPoint(p));
    }

    @Override
    public JTable getComponent() {
        return table;
    }

    @Override
    public boolean isEditing() {
        return table.isEditing();
    }

    @Override
    public Color getBackgroundAt(final Pair<Integer, Integer> position, final Component renderer) {
        return renderer.getBackground();
    }

    @Override
    public boolean isEditingCell(final Pair<Integer, Integer> position) {
        return isEditing() && position != null && table.getEditingColumn() == position.getSecond()
                && table.getEditingRow() == position.getFirst();
    }

    @Override
    public Component getCellRendererComponent(final Pair<Integer, Integer> position) {
        if (position == null) return null;
        int row = position.getFirst();
        int column = position.getSecond();
        boolean isSelected = table.isCellSelected(row, column);
        Object value = table.getValueAt(row, column);
        boolean focus = table.hasFocus();
        return ui.getCellRenderer(row, column).getTableCellRendererComponent(table, value, isSelected, focus, row,
                column);
    }

    @Override
    public Component getCellEditorComponent(final Pair<Integer, Integer> position) {
        return table.getEditorComponent();
    }
}
