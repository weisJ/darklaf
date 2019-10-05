package com.weis.darklaf.ui.tree;

import com.weis.darklaf.components.SelectableTreeNode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.TreeCellEditor;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.EventObject;

public class DarkTreeCellEditor extends DefaultCellEditor implements TreeCellEditor {

    private static final JCheckBox dummyCheckBox = new JCheckBox();
    private JTree tree;


    public DarkTreeCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(UIManager.getBorder("Tree.editorBorder"));
    }

    public DarkTreeCellEditor(final JCheckBox checkBox) {
        this((JToggleButton) checkBox);
    }

    public DarkTreeCellEditor(final JComboBox<?> comboBox) {
        super(comboBox);
        comboBox.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(final PopupMenuEvent e) {
                if (tree != null) tree.repaint();
            }

            @Override
            public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
                if (tree != null) tree.repaint();
            }

            @Override
            public void popupMenuCanceled(final PopupMenuEvent e) {

            }
        });
        setClickCountToStart(2);
    }

    public DarkTreeCellEditor(@NotNull final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        spinner.putClientProperty("JSpinner.isTreeCellEditor", Boolean.TRUE);
        setClickCountToStart(2);
        delegate = new EditorDelegate() {
            public void setValue(final Object value) {
                try {
                    var model = spinner.getModel();
                    if (model instanceof SpinnerNumberModel) {
                        spinner.setValue(NumberFormat.getInstance().parse(value.toString()));
                    } else if (model instanceof SpinnerDateModel) {
                        spinner.setValue(DateFormat.getInstance().parse(value.toString()));
                    } else {
                        spinner.setValue(value);
                    }
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }

            public Object getCellEditorValue() {
                return spinner.getValue();
            }

            public boolean shouldSelectCell(final EventObject anEvent) {
                if (anEvent instanceof MouseEvent) {
                    MouseEvent e = (MouseEvent) anEvent;
                    return e.getID() != MouseEvent.MOUSE_DRAGGED;
                }
                return true;
            }
        };
    }

    public DarkTreeCellEditor(@NotNull final JToggleButton toggleButton) {
        super(dummyCheckBox);
        editorComponent = toggleButton;
        delegate = new EditorDelegate() {
            public void setValue(final Object value) {
                boolean selected = Boolean.TRUE.equals(DarkTreeCellRenderer.unwrapBooleanIfPossible(value));
                toggleButton.setSelected(selected);
                if (value instanceof SelectableTreeNode) {
                    toggleButton.setText(((SelectableTreeNode)value).getLabel());
                }
            }

            public Object getCellEditorValue() {
                return toggleButton.isSelected();
            }
        };
        toggleButton.setFocusPainted(false);
        toggleButton.putClientProperty("JToggleButton.isTreeCellEditor", Boolean.TRUE);
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
        setClickCountToStart(1);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Component getTreeCellEditorComponent(final JTree tree, final Object value,
                                                final boolean isSelected, final boolean expanded,
                                                final boolean leaf, final int row) {
        this.tree = tree;
        delegate.setValue(value);
        if (editorComponent instanceof JComboBox) {
            ((JComboBox<?>) editorComponent).removeAllItems();
            ((JComboBox<Object>) editorComponent).addItem(value);
            ((JComboBox<?>) editorComponent).setSelectedItem(value);
        }
        editorComponent.setOpaque(false);
        return editorComponent;
    }

    public boolean isBooleanEditor(final JTree tree) {
        return editorComponent instanceof JToggleButton && DarkTreeCellRenderer.isBooleanRenderingEnabled(tree);
    }
}
