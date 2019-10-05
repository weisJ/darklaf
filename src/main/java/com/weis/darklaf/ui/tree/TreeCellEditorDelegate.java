package com.weis.darklaf.ui.tree;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.event.CellEditorListener;
import javax.swing.tree.TreeCellEditor;
import java.awt.*;
import java.util.EventObject;

public class TreeCellEditorDelegate implements TreeCellEditor {

    private final TreeCellEditor editor;

    @Contract(pure = true)
    public TreeCellEditorDelegate(final TreeCellEditor editor) {
        this.editor = editor;
    }

    @Override
    public Component getTreeCellEditorComponent(final JTree tree, final Object value, final boolean isSelected,
                                                final boolean expanded, final boolean leaf, final int row) {
        return editor.getTreeCellEditorComponent(tree, value, isSelected, expanded, leaf, row);
    }

    @Override
    public Object getCellEditorValue() {
        return editor.getCellEditorValue();
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        return editor.isCellEditable(anEvent);
    }

    @Override
    public boolean shouldSelectCell(final EventObject anEvent) {
        return editor.shouldSelectCell(anEvent);
    }

    @Override
    public boolean stopCellEditing() {
        return editor.stopCellEditing();
    }

    @Override
    public void cancelCellEditing() {
        editor.cancelCellEditing();
    }

    @Override
    public void addCellEditorListener(final CellEditorListener l) {
        editor.addCellEditorListener(l);
    }

    @Override
    public void removeCellEditorListener(final CellEditorListener l) {
        editor.removeCellEditorListener(l);
    }
}
