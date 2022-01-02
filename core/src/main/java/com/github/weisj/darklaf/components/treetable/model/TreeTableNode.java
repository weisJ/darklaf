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
package com.github.weisj.darklaf.components.treetable.model;

import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import javax.swing.tree.TreeNode;

public interface TreeTableNode extends TreeNode {

    List<TreeTableNode> getChildren();

    @Override
    default Enumeration<? extends TreeTableNode> children() {
        return Collections.enumeration(getChildren());
    }

    @Override
    TreeTableNode getParent();

    @Override
    default int getIndex(final TreeNode node) {
        if (!(node instanceof TreeTableNode)) return -1;
        return getIndex((TreeTableNode) node);
    }

    default int getIndex(final TreeTableNode node) {
        return getChildren().indexOf(node);
    }

    @Override
    default int getChildCount() {
        return getChildren().size();
    }

    @Override
    default TreeTableNode getChildAt(final int index) {
        if (index < 0 || index >= getChildCount()) return null;
        return getChildren().get(index);
    }

    @Override
    default boolean isLeaf() {
        return getChildCount() == 0;
    }

    List<Object> getColumns();

    default Object getValueAt(final int column) {
        return getColumns().get(column);
    }

    default Object getTreeValue() {
        return getColumns().get(0);
    }

    default int getColumnCount() {
        return getColumns().size();
    }

    default void setValueAt(final int column, final Object aValue) {
        getColumns().set(column, aValue);
    }
}
