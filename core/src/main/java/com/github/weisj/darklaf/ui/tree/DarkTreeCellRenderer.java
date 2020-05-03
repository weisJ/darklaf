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
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.PropertyValue;

/**
 * @author Jannis Weis
 */
public class DarkTreeCellRenderer extends DefaultTreeCellRenderer implements TreeRendererSupport {

    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorCheckBox> checkBoxRenderer = new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox(false));
    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorRadioButton> radioRenderer = new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorRadioButton(false));
    private final TreeRendererComponent rendererComponent = new TreeRendererComponent();
    private final TreeCellRenderer parent;

    public DarkTreeCellRenderer() {
        this(null);
    }

    public DarkTreeCellRenderer(final TreeCellRenderer parent) {
        this.parent = parent;
    }

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel,
                                                  final boolean expanded, final boolean leaf, final int row,
                                                  final boolean hasFocus) {
        boolean isFocused = DarkUIUtil.hasFocus(tree);
        Object val = unwrapBooleanIfPossible(value);
        if (val instanceof Boolean && isBooleanRenderingEnabled(tree)) {
            super.getTreeCellRendererComponent(tree, val, sel, expanded, leaf, row, hasFocus);
            Component comp = getBooleanRenderer(tree).getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
                                                                                   row, isFocused);
            rendererComponent.setComponentOrientation(tree.getComponentOrientation());
            comp.setComponentOrientation(tree.getComponentOrientation());
            comp.setFont(tree.getFont());
            rendererComponent.setRenderer(this);
            rendererComponent.setRenderComponent(comp);
            return rendererComponent;
        }
        Component comp;
        if (parent != null) {
            if (parent instanceof JLabel) {
                ((JLabel) parent).setIcon(null);
                ((JLabel) parent).setDisabledIcon(null);
            }
            comp = parent.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, isFocused);
        } else {
            setIcon(null);
            setDisabledIcon(null);
            comp = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, isFocused);
        }
        CellUtil.setupTreeForeground(comp, tree, sel);
        return comp;
    }

    public static Object unwrapBooleanIfPossible(final Object value) {
        Object val = value;
        if (val instanceof DefaultMutableTreeNode) {
            val = ((DefaultMutableTreeNode) val).getUserObject();
        }
        if (!(val instanceof Boolean)) {
            String str = String.valueOf(val);
            if (PropertyValue.TRUE.equals(str)) val = true;
            if (PropertyValue.FALSE.equals(str)) val = false;
        }
        return val;
    }

    protected static boolean isBooleanRenderingEnabled(final JTree tree) {
        return PropertyUtil.getBooleanProperty(tree, DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX);
    }

    protected DarkCellRendererToggleButton<?> getBooleanRenderer(final JTree table) {
        if (PropertyUtil.isPropertyEqual(table, DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE,
                                         DarkTreeUI.RENDER_TYPE_RADIOBUTTON)) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
