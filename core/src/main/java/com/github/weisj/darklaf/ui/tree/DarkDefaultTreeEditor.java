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
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.tree.*;

import com.github.weisj.darklaf.delegate.TreeCellEditorDelegate;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkDefaultTreeEditor extends DefaultTreeCellEditor {

    private static final DarkTreeCellEditor checkBoxEditor = new DarkTreeCellEditor(new JCheckBox());
    private static final DarkTreeCellEditor radioButtonEditor = new DarkTreeCellEditor(new JRadioButton());

    public DarkDefaultTreeEditor(final JTree tree, final DefaultTreeCellRenderer renderer) {
        this(tree, renderer, null);
    }

    public DarkDefaultTreeEditor(final JTree tree,
                                 final DefaultTreeCellRenderer renderer,
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
    protected TreeCellEditor createTreeCellEditor() {
        Border border = UIManager.getBorder("Tree.editorBorder");
        JTextField textField = new JTextField();
        textField.setBorder(border);
        textField.putClientProperty(DarkTextUI.KEY_IS_TREE_EDITOR, true);
        DefaultCellEditor editor = new DefaultCellEditor(textField);
        editor.setClickCountToStart(1);
        return editor;
    }

    @Override
    public Component getTreeCellEditorComponent(final JTree tree, final Object value, final boolean isSelected,
                                                final boolean expanded, final boolean leaf, final int row) {
        Component comp = super.getTreeCellEditorComponent(tree, value, isSelected, expanded, leaf, row);
        comp.setComponentOrientation(tree.getComponentOrientation());
        if (isBooleanRenderer(value, tree)) {
            ((Container) comp).remove(editingComponent);
            editingComponent = getBooleanEditor(tree).getTreeCellEditorComponent(tree, value, isSelected,
                                                                                 expanded, leaf, row);
            editingComponent.setFont(tree.getFont());
            ((Container) comp).add(editingComponent);
        }
        return comp;
    }

    protected DarkTreeCellEditor getBooleanEditor(final JTree table) {
        if (PropertyUtil.isPropertyEqual(table, DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE,
                                         DarkTreeUI.RENDER_TYPE_RADIOBUTTON)) {
            return radioButtonEditor;
        }
        return checkBoxEditor;
    }

    protected boolean isBooleanRenderer(final JTree tree, final int row) {
        TreePath path = tree.getPathForRow(row);
        if (path == null) return false;
        return isBooleanRenderer(path.getLastPathComponent(), tree);
    }

    protected boolean isBooleanRenderer(final Object value, final JTree tree) {
        return DarkTreeCellRendererDelegate.unwrapBooleanIfPossible(value) instanceof Boolean
               && DarkTreeCellRendererDelegate.isBooleanRenderingEnabled(tree);
    }

    @Override
    protected boolean canEditImmediately(final EventObject event) {
        JTree tree = getTree(event);
        if (tree != null) {
            JComponent button = getBooleanRendererComponent(tree);
            Point p = getPoint(event);
            if (p != null && button != null) {
                int row = tree.getRowForLocation(p.x, p.y);
                if (isBooleanRenderer(tree, row)) {
                    Rectangle bounds = tree.getRowBounds(row);
                    if (bounds != null) {
                        p.x -= bounds.x + button.getX();
                        p.y -= bounds.y + button.getY();
                        button.putClientProperty(DarkToggleButtonUI.KEY_CLEAR_HIT_AREA, true);
                        return button.contains(p);
                    }
                }
            }
        }
        return super.canEditImmediately(event);
    }

    protected JTree getTree(final EventObject eventObject) {
        return eventObject != null && eventObject.getSource() instanceof JTree ? (JTree) eventObject.getSource() : null;
    }

    protected Point getPoint(final EventObject eventObject) {
        return eventObject instanceof MouseEvent ? ((MouseEvent) eventObject).getPoint() : null;
    }

    protected JComponent getBooleanRendererComponent(final JTree tree) {
        DarkTreeUI ui = DarkUIUtil.getUIOfType(tree.getUI(), DarkTreeUI.class);
        if (ui == null) return null;
        TreeCellRenderer rend = ui.getCellRenderer();
        if (rend instanceof DarkTreeCellRendererDelegate) {
            DarkCellRendererToggleButton<?> booleanRend = ((DarkTreeCellRendererDelegate) rend).getBooleanRenderer(tree);
            return booleanRend.getButton();
        }
        return null;
    }
}
