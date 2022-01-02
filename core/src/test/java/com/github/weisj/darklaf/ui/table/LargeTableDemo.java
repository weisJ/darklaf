/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.table;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.table.AbstractTableModel;

import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class LargeTableDemo extends BaseComponentDemo {

    private static final int COLUMN_COUNT = 100;
    private static final int CELL_COUNT = 5000000;

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new LargeTableDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel holder = new JPanel();
        holder.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        JTable table = new JTable() {
            @Override
            public void columnMoved(final TableColumnModelEvent e) {
                int fromIndex = e.getFromIndex();
                int toIndex = e.getToIndex();
                int start = Math.max(Math.min(fromIndex, toIndex) - 1, 0);
                int end = Math.min(Math.max(fromIndex, toIndex) + 1, getColumnCount() - 1);
                Rectangle visible = getVisibleRect();
                for (int i = start; i <= end; i++) {
                    Rectangle cell = getCellRect(0, i, true);
                    cell.height = visible.height;
                    repaint(cell);
                }
            }
        };
        table.setModel(new DarkTableModel(CELL_COUNT / COLUMN_COUNT, COLUMN_COUNT));
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        holder.add(new JScrollPane(table));
        return holder;
    }

    @Override
    public String getName() {
        return "Large Table Demo";
    }

    static class DarkTableModel extends AbstractTableModel {

        int rows;
        int cols;

        public DarkTableModel(final int rows, final int cols) {
            this.rows = rows;
            this.cols = cols;
        }

        @Override
        public int getRowCount() {
            return rows;
        }

        @Override
        public int getColumnCount() {
            return cols;
        }

        @Override
        public Object getValueAt(final int rowIndex, final int columnIndex) {
            return rowIndex * cols + columnIndex;
        }
    }
}
