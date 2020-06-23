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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.hint.IndexedCellContainer;

public class TreeCellContainer implements IndexedCellContainer<JTree, Integer> {

    private final JTree tree;
    private final DarkTreeUI ui;

    public TreeCellContainer(final JTree tree, final DarkTreeUI ui) {
        this.tree = tree;
        this.ui = ui;
    }

    @Override
    public Rectangle getCellBoundsAt(final Integer position, final boolean isEditing) {
        return isEditing ? ui.getEditingComponent().getBounds() : tree.getRowBounds(position);
    }

    @Override
    public Integer getCellPosition(final Point p) {
        return tree.getClosestRowForLocation(p.x, p.y);
    }

    @Override
    public JTree getComponent() {
        return tree;
    }

    @Override
    public Color getBackgroundAt(final Integer position, final Component renderer) {
        if (position == null) return null;
        return CellUtil.getTreeBackground(tree, tree.isRowSelected(position), position);
    }

    @Override
    public boolean isEditingCell(final Integer row) {
        return isEditing() && row != null && ui.getEditingRow() == row;
    }

    @Override
    public Component getCellRendererComponent(final Integer row) {
        if (row == null) return null;
        TreeCellRenderer renderer = ui.getCellRenderer();
        TreePath path = tree.getPathForRow(row);
        boolean isExpanded = tree.isExpanded(row);
        boolean isLeaf = tree.getModel().isLeaf(path.getLastPathComponent());
        int leadIndex = tree.getLeadSelectionRow();
        return renderer.getTreeCellRendererComponent(tree, path.getLastPathComponent(),
                                                     tree.isRowSelected(row), isExpanded, isLeaf, row,
                                                     (leadIndex == row));
    }

    @Override
    public Component getCellEditorComponent(final Integer position) {
        return ui.getEditingComponent();
    }

    @Override
    public boolean isEditing() {
        return tree.isEditing();
    }
}
