package com.weis.darklaf.ui.tree;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.tree.DefaultTreeCellEditor;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.EventObject;

public class DarkDefaultTreeEditor extends DefaultTreeCellEditor {

    private static final DarkTreeCellEditor checkBoxEditor = new DarkTreeCellEditor(new JCheckBox());
    private static final DarkTreeCellEditor radioButtonEditor = new DarkTreeCellEditor(new JRadioButton());

    protected DarkTreeCellEditor getBooleanEditor(@NotNull final JTree table) {
        if ("radioButton".equals(table.getClientProperty("JTree.booleanRenderType"))) {
            return radioButtonEditor;
        }
        return checkBoxEditor;
    }

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

    @Override
    public boolean stopCellEditing() {
        return super.stopCellEditing();
    }

    @Override
    public Object getCellEditorValue() {
        return super.getCellEditorValue();
    }

    protected boolean isBooleanRenderer(final JTree tree, final int row) {
        var isBoolRenderer = realEditor instanceof DarkTreeCellEditor
                && ((DarkTreeCellEditor) realEditor).isBooleanEditor(tree);
        if (isBoolRenderer) return true;
        var path = tree.getPathForRow(row);
        return path != null
                && DarkTreeCellRenderer.unwrapBooleanIfPossible(path.getLastPathComponent()) instanceof Boolean;
    }
}
