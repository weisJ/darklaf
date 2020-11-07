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

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.delegate.TreeCellRendererDelegate;
import com.github.weisj.darklaf.ui.cell.*;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTreeCellRendererDelegate extends TreeCellRendererDelegate implements TreeRendererSupport {

    private final DarkCellRendererCheckBox checkBoxRenderer = new DarkCellRendererCheckBox(false);
    private final DarkCellRendererRadioButton radioRenderer = new DarkCellRendererRadioButton(false);
    private final DarkCellRendererTristateButton tristateRenderer = new DarkCellRendererTristateButton(false);
    private final TreeRendererComponent rendererComponent = new TreeRendererComponent();

    public DarkTreeCellRendererDelegate(final TreeCellRenderer renderer) {
        super(renderer);
    }

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean selected,
            final boolean expanded, final boolean leaf, final int row, final boolean hasFocus) {
        boolean isFocused = DarkUIUtil.hasFocus(tree);
        Object unwrapped = unwrapValue(value);
        Component renderer;
        if (unwrapped instanceof Boolean && isBooleanRenderingEnabled(tree)) {
            Component comp = getBooleanRenderer(tree).getTreeCellRendererComponent(tree, value, selected, expanded,
                    leaf, row, isFocused);
            renderer = prepareRendererComponent(tree, comp);
        } else if (unwrapped instanceof TristateState) {
            Component comp = getTristateRenderer(tree).getTreeCellRendererComponent(tree, value, selected, expanded,
                    leaf, row, isFocused);
            renderer = prepareRendererComponent(tree, comp);
        } else {
            TreeCellRenderer delegate = getDelegate();
            if (delegate instanceof DefaultTreeCellRenderer) {
                patchIcon(tree, (DefaultTreeCellRenderer) delegate, leaf, expanded);
            }
            renderer = super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, isFocused);
        }
        CellUtil.setupTreeForeground(renderer, tree, selected);
        CellUtil.setupTreeBackground(renderer, tree, selected, row);
        return renderer;
    }

    protected void patchIcon(final JTree tree, final DefaultTreeCellRenderer renderer, final boolean leaf,
            final boolean expanded) {
        Icon icon;
        if (leaf) {
            icon = renderer.getLeafIcon();
        } else if (expanded) {
            icon = renderer.getOpenIcon();
        } else {
            icon = renderer.getClosedIcon();
        }
        renderer.setIcon(icon);

        LookAndFeel laf = UIManager.getLookAndFeel();
        Icon disabledIcon = laf.getDisabledIcon(tree, icon);
        if (disabledIcon != null) icon = disabledIcon;
        renderer.setDisabledIcon(icon);
    }

    protected Component prepareRendererComponent(final JTree tree, final Component comp) {
        rendererComponent.setComponentOrientation(tree.getComponentOrientation());
        comp.setComponentOrientation(tree.getComponentOrientation());
        comp.setFont(tree.getFont());
        rendererComponent.setRenderer(this);
        rendererComponent.setRenderComponent(comp);
        return rendererComponent;
    }

    public static Object unwrapValue(final Object value) {
        Object val = value;
        if (val instanceof DefaultMutableTreeNode) {
            val = ((DefaultMutableTreeNode) val).getUserObject();
        }
        return val;
    }

    protected static boolean isBooleanRenderingEnabled(final JTree tree) {
        return PropertyUtil.getBooleanProperty(tree, DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX);
    }

    protected DarkCellRendererToggleButton<?> getBooleanRenderer(final JTree tree) {
        if (PropertyUtil.isPropertyEqual(tree, DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE,
                DarkTreeUI.RENDER_TYPE_RADIOBUTTON)) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }

    protected DarkCellRendererToggleButton<?> getTristateRenderer(final JTree tree) {
        return tristateRenderer;
    }

    @Override
    public Icon getIcon() {
        return getDelegate() instanceof JLabel ? ((JLabel) getDelegate()).getIcon() : null;
    }

    @Override
    public int getIconTextGap() {
        return getDelegate() instanceof JLabel ? ((JLabel) getDelegate()).getIconTextGap() : 0;
    }

    @Override
    public Dimension getPreferredSize() {
        return getDelegate() instanceof JComponent ? ((JComponent) getDelegate()).getPreferredSize() : null;
    }
}
