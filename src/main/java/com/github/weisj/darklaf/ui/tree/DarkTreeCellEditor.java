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
 */
package com.github.weisj.darklaf.ui.tree;

import com.github.weisj.darklaf.components.SelectableTreeNode;
import com.github.weisj.darklaf.util.DarkUIUtil;

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

/**
 * @author Jannis Weis
 */
public class DarkTreeCellEditor extends DefaultCellEditor implements TreeCellEditor {

    private static final JCheckBox dummyCheckBox = new JCheckBox();
    private JTree tree;
    private boolean adjustBoolValue;


    public DarkTreeCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(UIManager.getBorder("Tree.editorBorder"));
    }

    public DarkTreeCellEditor(final JCheckBox checkBox) {
        this((JToggleButton) checkBox);
    }

    public DarkTreeCellEditor(final JToggleButton toggleButton) {
        super(dummyCheckBox);
        editorComponent = toggleButton;
        delegate = new EditorDelegate() {
            public Object getCellEditorValue() {
                return toggleButton.isSelected();
            }

            public void setValue(final Object value) {
                boolean selected = Boolean.TRUE.equals(DarkTreeCellRenderer.unwrapBooleanIfPossible(value));
                toggleButton.setSelected(selected);
                if (value instanceof SelectableTreeNode) {
                    toggleButton.setText(((SelectableTreeNode) value).getLabel());
                }
            }
        };
        toggleButton.setFocusPainted(false);
        toggleButton.putClientProperty("JToggleButton.isTreeCellEditor", Boolean.TRUE);
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
        setClickCountToStart(1);
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

    public DarkTreeCellEditor(final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        spinner.putClientProperty("JSpinner.isTreeCellEditor", Boolean.TRUE);
        setClickCountToStart(2);
        delegate = new EditorDelegate() {
            public Object getCellEditorValue() {
                return spinner.getValue();
            }

            public void setValue(final Object value) {
                try {
                    SpinnerModel model = spinner.getModel();
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

            public boolean shouldSelectCell(final EventObject anEvent) {
                if (anEvent instanceof MouseEvent) {
                    MouseEvent e = (MouseEvent) anEvent;
                    return e.getID() != MouseEvent.MOUSE_DRAGGED;
                }
                return true;
            }
        };
    }

    public boolean isBooleanEditor(final JTree tree) {
        return editorComponent instanceof JToggleButton && DarkTreeCellRenderer.isBooleanRenderingEnabled(tree);
    }

    @Override
    public boolean isCellEditable(final EventObject anEvent) {
        adjustBoolValue = anEvent == null;
        return super.isCellEditable(anEvent);
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
        } else if (editorComponent instanceof JToggleButton) {
            ((JToggleButton) editorComponent).setSelected(!(((JToggleButton) editorComponent).isSelected()));
            SwingUtilities.invokeLater(tree::stopEditing);
        }
        editorComponent.setOpaque(false);
        editorComponent.setComponentOrientation(tree.getComponentOrientation());
        if (editorComponent instanceof JToggleButton) {
            if (DarkUIUtil.hasFocus(tree) || DarkUIUtil.hasFocus(editorComponent)) {
                editorComponent.setForeground(UIManager.getColor("Tree.selectionForeground"));
            } else {
                editorComponent.setForeground(UIManager.getColor("Tree.selectionForegroundInactive"));
            }
        }
        return editorComponent;
    }
}
