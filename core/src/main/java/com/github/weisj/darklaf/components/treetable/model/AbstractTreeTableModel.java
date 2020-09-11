/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.components.treetable.model;

import javax.swing.event.EventListenerList;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreePath;

public abstract class AbstractTreeTableModel implements TreeTableModel {
    protected TreeTableNode root;
    protected EventListenerList listenerList = new EventListenerList();

    private static final int CHANGED = 0;
    private static final int INSERTED = 1;
    private static final int REMOVED = 2;
    private static final int STRUCTURE_CHANGED = 3;

    public AbstractTreeTableModel(final TreeTableNode root) {
        this.root = root;
    }

    @Override
    public TreeTableNode getRoot() {
        return root;
    }

    @Override
    public boolean isLeaf(final Object node) {
        return getChildCount(node) == 0;
    }

    @Override
    public void valueForPathChanged(final TreePath path, final Object newValue) {}

    @Override
    public int getIndexOfChild(final Object parent, final Object child) {
        return 0;
    }

    @Override
    public void addTreeModelListener(final TreeModelListener l) {
        listenerList.add(TreeModelListener.class, l);
    }

    @Override
    public void removeTreeModelListener(final TreeModelListener l) {
        listenerList.remove(TreeModelListener.class, l);
    }

    private void fireTreeNode(
            final int changeType, final Object source, final Object[] path, final int[] childIndices,
            final Object[] children
    ) {
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = new TreeModelEvent(source, path, childIndices, children);
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                switch (changeType) {
                    case CHANGED:
                        ((TreeModelListener) listeners[i + 1]).treeNodesChanged(e);
                        break;
                    case INSERTED:
                        ((TreeModelListener) listeners[i + 1]).treeNodesInserted(e);
                        break;
                    case REMOVED:
                        ((TreeModelListener) listeners[i + 1]).treeNodesRemoved(e);
                        break;
                    case STRUCTURE_CHANGED:
                        ((TreeModelListener) listeners[i + 1]).treeStructureChanged(e);
                        break;
                    default:
                        break;
                }

            }
        }
    }

    protected void fireTreeNodesChanged(
            final Object source, final Object[] path, final int[] childIndices, final Object[] children
    ) {
        fireTreeNode(CHANGED, source, path, childIndices, children);
    }

    protected void fireTreeNodesInserted(
            final Object source, final Object[] path, final int[] childIndices, final Object[] children
    ) {
        fireTreeNode(INSERTED, source, path, childIndices, children);
    }

    protected void fireTreeNodesRemoved(
            final Object source, final Object[] path, final int[] childIndices, final Object[] children
    ) {
        fireTreeNode(REMOVED, source, path, childIndices, children);
    }

    protected void fireTreeStructureChanged(
            final Object source, final Object[] path, final int[] childIndices, final Object[] children
    ) {
        fireTreeNode(STRUCTURE_CHANGED, source, path, childIndices, children);
    }
}
