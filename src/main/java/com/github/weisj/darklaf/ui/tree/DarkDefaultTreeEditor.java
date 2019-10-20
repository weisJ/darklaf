/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.tree;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.tree.DefaultTreeCellEditor;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.EventObject;

/**
 * @author Jannis Weis
 */
public class DarkDefaultTreeEditor extends DefaultTreeCellEditor {

    private static final DarkTreeCellEditor checkBoxEditor = new DarkTreeCellEditor(new JCheckBox());
    private static final DarkTreeCellEditor radioButtonEditor = new DarkTreeCellEditor(new JRadioButton());

    public DarkDefaultTreeEditor(final JTree tree, final DarkTreeCellRenderer renderer) {
        this(tree, renderer, null);
    }

    public DarkDefaultTreeEditor(final JTree tree, final DarkTreeCellRenderer renderer,
                                 final DarkTreeCellEditor editor) {
        super(tree, renderer, editor);
        realEditor = new TreeCellEditorDelegate(realEditor) {
            @Override
            public Object getCellEditorValue() {
                if (isBooleanRenderer(tree, lastRow)) {
                    return getBooleanEditor(tree).getCellEditorValue();
                }
                return super.getCellEditorValue();
            }

            @Override
            public boolean isCellEditable(final EventObject anEvent) {
                if (isBooleanRenderer(tree, tree.getLeadSelectionRow())) {
                    return getBooleanEditor(tree).isCellEditable(anEvent);
                }
                return super.isCellEditable(anEvent);
            }
        };
    }

    protected boolean isBooleanRenderer(final JTree tree, final int row) {
        var isBoolRenderer = realEditor instanceof DarkTreeCellEditor
                && ((DarkTreeCellEditor) realEditor).isBooleanEditor(tree);
        if (isBoolRenderer) return true;
        var path = tree.getPathForRow(row);
        return path != null
                && DarkTreeCellRenderer.unwrapBooleanIfPossible(path.getLastPathComponent()) instanceof Boolean;
    }

    protected DarkTreeCellEditor getBooleanEditor(@NotNull final JTree table) {
        if ("radioButton".equals(table.getClientProperty("JTree.booleanRenderType"))) {
            return radioButtonEditor;
        }
        return checkBoxEditor;
    }

    @Override
    public Component getTreeCellEditorComponent(final JTree tree, final Object value, final boolean isSelected,
                                                final boolean expanded, final boolean leaf, final int row) {
        var comp = super.getTreeCellEditorComponent(tree, value, isSelected, expanded, leaf, row);
        comp.setComponentOrientation(tree.getComponentOrientation());
        if (isBooleanRenderer(tree, row)) {
            ((Container) comp).remove(editingComponent);
            editingComponent = getBooleanEditor(tree).getTreeCellEditorComponent(tree, value, isSelected,
                                                                                 expanded, leaf, row);
            editingComponent.setFont(tree.getFont());
            ((Container) comp).add(editingComponent);
        }
        return comp;
    }

    @Override
    public Object getCellEditorValue() {
        return super.getCellEditorValue();
    }

    @Override
    public boolean stopCellEditing() {
        return super.stopCellEditing();
    }

    @Override
    protected boolean canEditImmediately(final EventObject event) {
        if (event != null && event.getSource() instanceof JTree) {
            var tree = ((JTree) event.getSource());
            var renderer = tree.getCellRenderer();
            if (event instanceof MouseEvent) {
                var p = ((MouseEvent) event).getPoint();
                int row = tree.getRowForLocation(p.x, p.y);
                if (isBooleanRenderer(tree, row) && renderer instanceof DarkTreeCellRenderer) {
                    var bounds = tree.getRowBounds(row);
                    if (bounds != null) {
                        var rend = (DarkTreeCellRenderer) renderer;
                        var booleanRend = rend.getBooleanRenderer(tree);
                        var button = booleanRend.getButton();

                        p.x -= bounds.x + button.getX();
                        p.y -= bounds.y + button.getY();
                        return button.contains(p);
                    }
                }
            }
        }
        return super.canEditImmediately(event);
    }
}
