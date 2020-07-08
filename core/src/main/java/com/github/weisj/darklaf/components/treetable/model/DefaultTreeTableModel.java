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
package com.github.weisj.darklaf.components.treetable.model;

import com.github.weisj.darklaf.util.DarkUIUtil;

public class DefaultTreeTableModel extends AbstractTreeTableModel {

    private final String[] headers;
    private Class<?>[] columnClasses;

    public DefaultTreeTableModel(final TreeTableNode root, final String[] headers) {
        super(root);
        this.headers = headers;
    }

    public void setColumnClasses(final Class<?>[] columnClasses) {
        this.columnClasses = columnClasses;
    }

    @Override
    public int getColumnCount() {
        return headers.length;
    }

    @Override
    public String getColumnName(final int column) {
        return headers[column];
    }

    @Override
    public Class<?> getColumnClass(final int column) {
        if (column == 0) {
            return TreeTableModel.class;
        } else if (columnClasses != null && column < columnClasses.length) {
            return columnClasses[column];
        } else {
            return Object.class;
        }
    }

    @Override
    public Object getValueAt(final Object node, final int column) {
        TreeTableNode treeTableNode = DarkUIUtil.nullableCast(TreeTableNode.class, node);
        if (treeTableNode == null) return null;
        return treeTableNode.getValueAt(column);
    }

    @Override
    public boolean isCellEditable(final Object node, final int column) {
        return column != 0;
    }

    @Override
    public void setValueAt(final Object aValue, final Object node, final int column) {
        TreeTableNode treeTableNode = DarkUIUtil.nullableCast(TreeTableNode.class, node);
        if (treeTableNode != null) {
            treeTableNode.setValueAt(column, aValue);
        }
    }

    @Override
    public Object getChild(final Object parent, final int index) {
        TreeTableNode treeTableNode = DarkUIUtil.nullableCast(TreeTableNode.class, parent);
        return treeTableNode != null ? treeTableNode.getChildAt(index) : null;
    }

    @Override
    public int getChildCount(final Object parent) {
        TreeTableNode treeTableNode = DarkUIUtil.nullableCast(TreeTableNode.class, parent);
        return treeTableNode != null ? treeTableNode.getChildCount() : 0;
    }
}
