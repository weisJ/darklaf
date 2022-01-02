/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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

import javax.swing.*;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;

public class DefaultTreeTableSelectionModel extends DefaultTreeSelectionModel implements DelegatingListSelectionModel {

    private final JTree tree;
    private final ListSelectionModel bridgeModel;

    public DefaultTreeTableSelectionModel(final JTree tree) {
        this.tree = tree;
        bridgeModel = new TreeTableListSelectionModel();
    }

    @Override
    public ListSelectionModel getListDelegate() {
        return bridgeModel;
    }

    protected class TreeTableListSelectionModel implements DelegatingListSelectionModel {

        @Override
        public ListSelectionModel getListDelegate() {
            return listSelectionModel;
        }

        @Override
        public void setSelectionInterval(final int index0, final int index1) {
            setSelectionPaths(getTreePaths(index0, index1));
        }

        @Override
        public void addSelectionInterval(final int index0, final int index1) {
            addSelectionPaths(getTreePaths(index0, index1));
        }

        protected TreePath[] getTreePaths(final int index0, final int index1) {
            int start = Math.min(index0, index1);
            int end = Math.max(index0, index1);
            TreePath[] paths = new TreePath[end - start + 1];
            for (int i = start; i <= end; i++) {
                paths[i - start] = tree.getPathForRow(i);
            }
            return paths;
        }

        @Override
        public void removeSelectionInterval(final int index0, final int index1) {
            removeSelectionPaths(getTreePaths(index0, index1));
        }

        @Override
        public void setLeadSelectionIndex(final int index) {
            tree.setLeadSelectionPath(tree.getPathForRow(index));
        }

        @Override
        public int getLeadSelectionIndex() {
            return getLeadSelectionRow();
        }

        @Override
        public void setAnchorSelectionIndex(final int index) {
            tree.setAnchorSelectionPath(tree.getPathForRow(index));
        }

        @Override
        public int getAnchorSelectionIndex() {
            return tree.getRowForPath(tree.getAnchorSelectionPath());
        }
    }
}
