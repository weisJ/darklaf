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
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import com.github.weisj.darklaf.components.treetable.model.AbstractTreeTableModel;
import com.github.weisj.darklaf.components.treetable.model.DefaultTreeTableSelectionModel;
import com.github.weisj.darklaf.components.treetable.model.TreeTableModel;
import com.github.weisj.darklaf.ui.cell.hint.CellHintPopupListener;
import com.github.weisj.darklaf.ui.tree.DarkTreeUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class JTreeTable extends JTable implements TreeSelectionListener {

    private final TreeTableTree tree;

    public JTreeTable(final AbstractTreeTableModel treeTableModel) {
        DefaultTreeTableCellRenderer treeCellRenderer = new DefaultTreeTableCellRenderer(this, treeTableModel);
        tree = treeCellRenderer.getTree();

        add(tree);

        tree.putClientProperty(DarkTreeUI.KEY_IS_TABLE_TREE, true);

        DefaultTreeTableSelectionModel selectionModel = new DefaultTreeTableSelectionModel(tree);
        tree.setSelectionModel(selectionModel);
        setSelectionModel(selectionModel);
        tree.addTreeSelectionListener(this);

        setDefaultRenderer(TreeTableModel.class, treeCellRenderer);
        super.setModel(new TreeTableModelAdapter(treeTableModel, tree));
        setShowHorizontalLines(false);
    }

    @Override
    public void doLayout() {
        super.doLayout();
        int grid = getShowVerticalLines() ? getIntercellSpacing().width : 0;
        tree.setBounds(0, 0, getColumnModel().getColumn(0).getWidth() - grid, getHeight());
    }

    @Override
    public void repaint(final Rectangle r) {
        super.repaint(r);
        SwingUtilities.invokeLater(() -> {
            DarkTreeUI ui = DarkUIUtil.getUIOfType(tree.getUI(), DarkTreeUI.class);
            if (ui != null) {
                CellHintPopupListener<JTree, ?> listener = ui.getPopupListener();
                if (listener != null) listener.repaint();
            }
        });
    }

    @Override
    public int getComponentCount() {
        return 0;
    }

    @Override
    protected void processEvent(final AWTEvent e) {
        if (e instanceof InputEvent) {
            if (e instanceof KeyEvent) {
                if (getColumnModel().getSelectionModel().getLeadSelectionIndex() == 0
                    && !((KeyEvent) e).isShiftDown()) {
                    tree.processEvent(e);
                }
            }
            if (((InputEvent) e).isConsumed()) return;
        }
        super.processEvent(e);
    }

    @Override
    public void valueChanged(final TreeSelectionEvent e) {
        repaint(0, 0, getColumnModel().getColumn(0).getWidth(), getHeight());
    }
}
