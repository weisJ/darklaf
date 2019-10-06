package com.weis.darklaf.ui.tree;

import com.weis.darklaf.ui.cell.DarkCellRendererToggleButton;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import java.awt.*;

public class DarkTreeCellRenderer extends DefaultTreeCellRenderer implements TreeRendererSupport {

    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorCheckBox> checkBoxRenderer =
            new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox());
    private final DarkCellRendererToggleButton<DarkCellRendererToggleButton.CellEditorRadioButton> radioRenderer =
            new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorRadioButton());
    private TreeRendererComponent rendererComponent = new TreeRendererComponent();

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel,
                                                  final boolean expanded, final boolean leaf, final int row,
                                                  final boolean hasFocus) {
        var val = unwrapBooleanIfPossible(value);
        if (val instanceof Boolean && isBooleanRenderingEnabled(tree)) {
            super.getTreeCellRendererComponent(tree, val, sel, expanded, leaf, row, hasFocus);
            var comp = getBooleanRenderer(tree).getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
                                                                             row, hasFocus);
            rendererComponent.setComponentOrientation(tree.getComponentOrientation());
            comp.setComponentOrientation(tree.getComponentOrientation());
            comp.setFont(tree.getFont());
            rendererComponent.setRenderer(this);
            rendererComponent.setRenderComponent(comp);
            return rendererComponent;
        }

        return super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
    }

    public static Object unwrapBooleanIfPossible(final Object value) {
        var val = value;
        if (val instanceof DefaultMutableTreeNode) {
            val = ((DefaultMutableTreeNode) val).getUserObject();
        }
        if (!(val instanceof Boolean)) {
            var str = val.toString();
            if ("true".equals(str)) val = true;
            if ("false".equals(str)) val = false;
        }
        return val;
    }

    protected static boolean isBooleanRenderingEnabled(@NotNull final JTree tree) {
        return Boolean.TRUE.equals(tree.getClientProperty("JTree.renderBooleanAsCheckBox"));
    }

    protected DarkCellRendererToggleButton getBooleanRenderer(@NotNull final JTree table) {
        if ("radioButton".equals(table.getClientProperty("JTree.booleanRenderType"))) {
            return radioRenderer;
        }
        return checkBoxRenderer;
    }
}
