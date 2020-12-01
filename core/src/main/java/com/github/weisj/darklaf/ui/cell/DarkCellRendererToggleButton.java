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
package com.github.weisj.darklaf.ui.cell;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import com.github.weisj.darklaf.components.tree.LabeledTreeNode;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;
import com.github.weisj.darklaf.ui.tree.DarkTreeCellRendererDelegate;
import com.github.weisj.darklaf.util.DarkUIUtil;

/** @author Jannis Weis */
public class DarkCellRendererToggleButton<T extends JToggleButton & CellToggleButton>
        implements TableCellRenderer, TreeCellRenderer, SwingConstants {

    private final T toggleButton;
    private final Icon checkIcon;
    private final Icon checkDisabledIcon;
    private final Icon checkSelectedIcon;
    private final Icon noCheck;
    private final Border border = new DarkCellBorderUIResource();

    public DarkCellRendererToggleButton(final T toggleButton) {
        this.toggleButton = toggleButton;
        checkIcon = UIManager.getIcon("Check.icon");
        checkDisabledIcon = UIManager.getIcon("Check.disabled.icon");
        checkSelectedIcon = UIManager.getIcon("Check.selected.icon");
        noCheck = UIManager.getIcon("Check.deselected.icon");
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected,
            final boolean focus, final int row, final int column) {
        if (value instanceof Boolean) {
            toggleButton.setSelected((Boolean) value);
        }
        toggleButton.setHasFocus(focus);
        toggleButton.setBorder(border);

        updateIcon(table.isCellEditable(row, column), isSelected && DarkUIUtil.hasFocus(table));
        toggleButton.setEnabled(table.isEnabled());

        return toggleButton;
    }

    public void updateIcon(final boolean editable, final boolean cellSelected) {
        if (!editable) {
            toggleButton.setSelectedIcon(cellSelected ? checkSelectedIcon : checkIcon);
            toggleButton.setDisabledSelectedIcon(checkDisabledIcon);
            toggleButton.setIcon(noCheck);
        } else {
            toggleButton.setIcon(null);
            toggleButton.setDisabledSelectedIcon(null);
            toggleButton.setSelectedIcon(null);
        }
    }

    @Override
    public T getTreeCellRendererComponent(final JTree tree, final Object value, final boolean selected,
            final boolean expanded, final boolean leaf, final int row, final boolean focus) {
        if (value instanceof Boolean) {
            toggleButton.setSelected((Boolean) value);
        } else {
            setValue(value);
            if (value instanceof LabeledTreeNode) {
                toggleButton.setText(((LabeledTreeNode) value).getLabel());
            }
        }

        updateIcon(tree.isEditable() && tree.isPathEditable(tree.getPathForRow(row)),
                selected && DarkUIUtil.hasFocus(tree));

        toggleButton.setEnabled(tree.isEnabled());
        toggleButton.setHasFocus(false);
        toggleButton.setHorizontalAlignment(tree.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);
        return toggleButton;
    }



    protected void setValue(final Object value) {
        toggleButton.setSelected(Boolean.TRUE.equals(DarkTreeCellRendererDelegate.unwrapValue(value)));
    }

    public T getButton() {
        return toggleButton;
    }

    public static class CellCheckBox extends JCheckBox implements CellRenderer, CellToggleButton {

        private boolean hasFocus;

        public CellCheckBox(final boolean opaque) {
            setOpaque(opaque);
            setHorizontalAlignment(CENTER);
            setBorderPainted(true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TREE_EDITOR, true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TABLE_EDITOR, true);
        }

        public void setHasFocus(final boolean hasFocus) {
            this.hasFocus = hasFocus;
        }

        @Override
        public boolean hasFocus() {
            return hasFocus || super.hasFocus();
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }

    public static class CellRadioButton extends JRadioButton implements CellRenderer, CellToggleButton {

        private boolean hasFocus;

        public CellRadioButton(final boolean opaque) {
            setOpaque(opaque);
            setHorizontalAlignment(CENTER);
            setBorderPainted(true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TREE_EDITOR, true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TABLE_EDITOR, true);
        }

        @Override
        public void setSelected(final boolean b) {
            super.setSelected(b);
        }

        public void setHasFocus(final boolean hasFocus) {
            this.hasFocus = hasFocus;
        }

        @Override
        public boolean hasFocus() {
            return hasFocus || super.hasFocus();
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }

    public static class CellTristateButton extends TristateCheckBox implements CellRenderer, CellToggleButton {

        private boolean hasFocus;

        public CellTristateButton(final boolean opaque) {
            setOpaque(opaque);
            setHorizontalAlignment(CENTER);
            setBorderPainted(true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TREE_EDITOR, true);
            putClientProperty(ToggleButtonConstants.KEY_IS_TABLE_EDITOR, true);
        }

        @Override
        public boolean hasFocus() {
            return hasFocus || super.hasFocus();
        }

        @Override
        public void setHasFocus(final boolean hasFocus) {
            this.hasFocus = hasFocus;
        }

        @Override
        public boolean isFocusOwner() {
            return super.hasFocus();
        }
    }
}
