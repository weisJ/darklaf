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
package theme;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.table.DefaultTableModel;

class MapTableModel extends DefaultTableModel {

    private final LinkedHashMap<Object, Object> values;
    private final boolean keyEditable;

    MapTableModel(final LinkedHashMap<Object, Object> values, final boolean keyEditable) {
        this.values = values;
        this.keyEditable = keyEditable;
    }

    @Override
    public int getRowCount() {
        return values != null ? values.size() : 0;
    }

    @Override
    public int getColumnCount() {
        return 2;
    }

    @Override
    public String getColumnName(final int columnIndex) {
        return columnIndex == 0 ? "Key" : "Value";
    }

    @Override
    public Class<?> getColumnClass(final int columnIndex) {
        return columnIndex == 0 ? String.class : Object.class;
    }

    @Override
    public boolean isCellEditable(final int rowIndex, final int columnIndex) {
        return columnIndex == 1 || keyEditable;
    }

    @Override
    public Object getValueAt(final int rowIndex, final int columnIndex) {
        if (columnIndex == 0) {
            return getKeyByIndex(rowIndex);
        } else {
            return values.get(getKeyByIndex(rowIndex));
        }
    }

    @Override
    public void insertRow(final int row, final Vector rowData) {
        values.put(rowData.get(0), rowData.get(1));
        fireTableRowsInserted(getRowCount() - 1, getRowCount() - 1);
    }

    @Override
    public void removeRow(final int row) {
        values.remove(getKeyByIndex(row));
        fireTableRowsDeleted(row, row);
    }

    @Override
    public void addColumn(final Object columnName, final Vector columnData) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setValueAt(final Object aValue, final int rowIndex, final int columnIndex) {
        if (columnIndex == 0) {
            if (!keyEditable) return;
            @SuppressWarnings("unchecked")
            Map.Entry<Object, Object>[] entries =
                    (Map.Entry<Object, Object>[]) values.entrySet().toArray(new Map.Entry[0]);
            for (int i = rowIndex; i < entries.length; i++) {
                values.remove(entries[i].getKey());
            }
            values.put(aValue, entries[rowIndex].getValue());
            for (int i = rowIndex + 1; i < entries.length; i++) {
                values.remove(entries[i].getKey());
            }
            fireTableCellUpdated(rowIndex, columnIndex);
        } else {
            values.put(getKeyByIndex(rowIndex), aValue);
            fireTableCellUpdated(rowIndex, columnIndex);
        }
    }

    public String getKeyByIndex(final int index) {
        return values.keySet().toArray(new Object[0])[index].toString();
    }
}
