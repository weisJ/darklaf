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

import javax.swing.*;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.components.treetable.model.TreeTableModel;

public class TreeTableModelAdapter extends AbstractTableModel {

    private final JTree tree;
    private final TreeTableModel treeTableModel;

    public TreeTableModelAdapter(final TreeTableModel treeTableModel, final JTree tree) {
        this.tree = tree;
        this.treeTableModel = treeTableModel;

        tree.addTreeExpansionListener(new TreeExpansionListener() {
            public void treeExpanded(final TreeExpansionEvent event) {
                TreePath path = event.getPath();
                int start = tree.getRowForPath(path);
                int length = ((TreeNode) path.getLastPathComponent()).getChildCount();
                int selection = tree.getLeadSelectionRow();
                fireTableRowsInserted(start, start + length);
                tree.setSelectionRow(selection);
            }

            public void treeCollapsed(final TreeExpansionEvent event) {
                TreePath path = event.getPath();
                int start = tree.getRowForPath(path);
                int length = ((TreeNode) path.getLastPathComponent()).getChildCount();
                int selection = tree.getLeadSelectionRow();
                fireTableRowsDeleted(start, start + length);
                tree.setSelectionRow(selection);
            }
        });
    }

    public int getColumnCount() {
        return treeTableModel.getColumnCount();
    }

    public String getColumnName(final int column) {
        return treeTableModel.getColumnName(column);
    }

    public Class<?> getColumnClass(final int column) {
        return treeTableModel.getColumnClass(column);
    }

    public int getRowCount() {
        return tree.getRowCount();
    }

    protected Object nodeForRow(final int row) {
        TreePath treePath = tree.getPathForRow(row);
        return treePath.getLastPathComponent();
    }

    public Object getValueAt(final int row, final int column) {
        return treeTableModel.getValueAt(nodeForRow(row), column);
    }

    public boolean isCellEditable(final int row, final int column) {
        return treeTableModel.isCellEditable(nodeForRow(row), column);
    }

    public void setValueAt(final Object value, final int row, final int column) {
        treeTableModel.setValueAt(value, nodeForRow(row), column);
    }
}
