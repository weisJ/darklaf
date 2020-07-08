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
package com.github.weisj.darklaf.components.treetable;

import java.awt.*;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.ui.tree.DarkTreeUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class DefaultTreeTableCellRenderer extends JComponent implements TableCellRenderer {

    private final JTreeTable treeTable;
    private final RendererTree rendererTree = new RendererTree();
    private int paintingRow;

    public DefaultTreeTableCellRenderer(final JTreeTable treeTable, final TreeModel model) {
        this.treeTable = treeTable;
        rendererTree.setRowHeight(rendererTree.getRowHeight());
        rendererTree.setModel(model);
        rendererTree.setBounds(0, 0, Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    @Override
    protected void paintComponent(final Graphics g) {
        DarkTreeUI ui = DarkUIUtil.getUIOfType(rendererTree.getUI(), DarkTreeUI.class);
        if (ui != null) {
            ui.paintRow(g, paintingRow);
        }
    }

    public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected,
                                                   final boolean hasFocus, final int row, final int column) {
        paintingRow = row;
        rendererTree.setFocus(hasFocus || table.hasFocus());
        rendererTree.setSelectable(!hasFocus);
        return this;
    }

    public TreeTableTree getTree() {
        return rendererTree;
    }

    protected class RendererTree extends TreeTableTree {
        private boolean focus;
        private boolean selectable;

        public void setRowHeight(final int rowHeight) {
            if (rowHeight > 0) {
                super.setRowHeight(rowHeight);
                if (treeTable != null && treeTable.getRowHeight() != rowHeight) {
                    treeTable.setRowHeight(getRowHeight());
                }
            }
        }

        @Override
        public void repaint(final int x, final int y, final int width, final int height) {
            treeTable.repaint(x, y, treeTable.getColumnModel().getColumn(0).getWidth(), height);
        }

        @Override
        public boolean isRowSelected(final int row) {
            return selectable && super.isRowSelected(row);
        }

        @Override
        public boolean isPathSelected(final TreePath path) {
            return selectable && super.isPathSelected(path);
        }

        @Override
        public void scrollRectToVisible(final Rectangle aRect) {
            treeTable.scrollRectToVisible(aRect);
        }

        public void setFocus(final boolean focus) {
            this.focus = focus;
        }

        public void setSelectable(final boolean selectable) {
            this.selectable = selectable;
        }

        @Override
        public boolean hasFocus() {
            return focus;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }
}
