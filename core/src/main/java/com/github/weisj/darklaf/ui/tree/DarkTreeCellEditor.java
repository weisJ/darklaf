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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.TreeCellEditor;

import com.github.weisj.darklaf.components.tree.LabeledTreeNode;
import com.github.weisj.darklaf.components.tristate.TristateButtonModel;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.combobox.ComboBoxConstants;
import com.github.weisj.darklaf.ui.spinner.SpinnerConstants;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;

/**
 * @author Jannis Weis
 */
public class DarkTreeCellEditor extends DefaultCellEditor implements TreeCellEditor, FocusListener {

    private static final JCheckBox dummyCheckBox = new JCheckBox();
    private JTree tree;

    public DarkTreeCellEditor(final JTextField textField) {
        super(textField);
        textField.setBorder(UIManager.getBorder("Tree.editorBorder"));
        textField.addFocusListener(this);
        textField.putClientProperty(DarkTextUI.KEY_IS_TREE_EDITOR, true);
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
                Object unwrapped = DarkTreeCellRendererDelegate.unwrapValue(value);
                if (unwrapped instanceof TristateState && toggleButton instanceof TristateCheckBox) {
                    ((TristateCheckBox) toggleButton).setState((TristateState) unwrapped);
                } else {
                    toggleButton.setSelected(Boolean.TRUE.equals(unwrapped));
                }
                if (value instanceof LabeledTreeNode) {
                    toggleButton.setText(((LabeledTreeNode) value).getLabel());
                }
            }
        };
        toggleButton.setFocusPainted(false);
        toggleButton.putClientProperty(ToggleButtonConstants.KEY_IS_TREE_EDITOR, Boolean.TRUE);
        toggleButton.addActionListener(delegate);
        toggleButton.setRequestFocusEnabled(false);
        toggleButton.addFocusListener(this);
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
        comboBox.putClientProperty(ComboBoxConstants.KEY_IS_TREE_EDITOR, Boolean.TRUE);
        comboBox.addFocusListener(this);
        setClickCountToStart(2);
    }

    public DarkTreeCellEditor(final JSpinner spinner) {
        super(dummyCheckBox);
        editorComponent = spinner;
        editorComponent.putClientProperty(SpinnerConstants.KEY_IS_TREE_EDITOR, Boolean.TRUE);
        editorComponent.addFocusListener(this);
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

    @SuppressWarnings("unchecked")
    @Override
    public Component getTreeCellEditorComponent(
            final JTree tree, final Object value, final boolean isSelected, final boolean expanded, final boolean leaf,
            final int row
    ) {
        this.tree = tree;
        delegate.setValue(value);
        if (editorComponent instanceof JComboBox) {
            ((JComboBox<?>) editorComponent).removeAllItems();
            ((JComboBox<Object>) editorComponent).addItem(value);
            ((JComboBox<?>) editorComponent).setSelectedItem(value);
        } else if (editorComponent instanceof JToggleButton) {
            ButtonModel model = ((JToggleButton) editorComponent).getModel();
            if (model instanceof TristateButtonModel) {
                ((TristateButtonModel) model).iterateState();
            } else {
                model.setSelected(!model.isSelected());
            }
            SwingUtilities.invokeLater(tree::stopEditing);
        }
        editorComponent.setOpaque(true);
        editorComponent.setComponentOrientation(tree.getComponentOrientation());
        CellUtil.setupTreeBackground(editorComponent, tree, false, row);
        CellUtil.setupTreeForeground(editorComponent, tree, false);
        return editorComponent;
    }

    protected void updateFocus(final FocusEvent e) {
        CellUtil.setupTreeForeground(editorComponent, tree, false);
    }

    @Override
    public void focusGained(final FocusEvent e) {
        updateFocus(null);
    }

    @Override
    public void focusLost(final FocusEvent e) {
        updateFocus(e);
    }
}
