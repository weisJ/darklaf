/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import com.github.weisj.darklaf.ui.table.TableConstants;
import com.github.weisj.darklaf.ui.tree.DarkTreeUI;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkBooleanCellRenderer implements TableCellRenderer, TreeCellRenderer {

    private final DarkCellRendererCheckBox checkBoxRenderer;
    private final DarkCellRendererRadioButton radioRenderer;

    public DarkBooleanCellRenderer(final boolean opaque) {
        checkBoxRenderer = new DarkCellRendererCheckBox(opaque);
        radioRenderer = new DarkCellRendererRadioButton(opaque);
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected,
            final boolean hasFocus, final int row, final int column) {
        return getBooleanRenderer(table)
                .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
    }

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean selected,
            final boolean expanded,
            final boolean leaf, final int row, final boolean hasFocus) {
        return getBooleanRenderer(tree)
                .getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
    }

    protected ComponentBasedTableCellRenderer getBooleanRenderer(final JTable table) {
        return getRenderer(PropertyUtil.isPropertyEqual(table,
                TableConstants.KEY_BOOLEAN_RENDER_TYPE, CellConstants.RENDER_TYPE_RADIOBUTTON));
    }

    protected ComponentBasedTreeCellRenderer getBooleanRenderer(final JTree tree) {
        return getRenderer(PropertyUtil.isPropertyEqual(tree,
                DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE, CellConstants.RENDER_TYPE_RADIOBUTTON));
    }

    private DarkCellRendererToggleButton<?> getRenderer(final boolean useRadioButton) {
        return useRadioButton ? radioRenderer : checkBoxRenderer;
    }

    public JComponent getRendererComponent(final JTree tree) {
        return getBooleanRenderer(tree).getRendererComponent();
    }

    public JComponent getRendererComponent(final JTable table) {
        return getBooleanRenderer(table).getRendererComponent();
    }
}
