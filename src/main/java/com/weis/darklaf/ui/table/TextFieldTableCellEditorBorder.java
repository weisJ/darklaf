package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.text.DarkTextFieldUI;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class TextFieldTableCellEditorBorder extends DarkTableCellBorder {

    @Override
    public void paintBorder(@NotNull final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(DarkTextFieldUI.getBorderColor(false, false, true, true));
        var table = DarkUIUtil.getParentOfType(JTable.class, c);
        if (table != null) {
            if (!table.getShowHorizontalLines()) {
                g.fillRect(0, 0, width, 1);
                g.fillRect(0, height - 1, width, 1);
            }
            if (!table.getShowVerticalLines()) {
                g.fillRect(0, 0, 1, height);
                g.fillRect(width - 1, 0, 1, height);
            } else if (isInWrapper(c)) {
                if (c.getParent().getComponentOrientation().isLeftToRight()) {
                    g.fillRect(0, 0, 1, height);
                } else {
                    g.fillRect(width - 1, 0, 1, height);
                }
            }
        } else {
            DarkUIUtil.drawRect(g, x, y, width, height, 1);
        }
    }

    protected static boolean isInWrapper(@NotNull final Component c) {
        return c.getParent() instanceof DarkTableCellEditor.IconWrapper;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        var ins = super.getBorderInsets();
        if (isInWrapper(c)) {
            if (parentLTR(c)) {
                ins.left -= ((DarkTableCellEditor.IconWrapper) c.getParent()).getIconCompGap();
            } else {
                ins.right -= ((DarkTableCellEditor.IconWrapper) c.getParent()).getIconCompGap();
            }
        } else if (isListEditor(c)) {
            var renderer = ((JList) c.getParent()).getCellRenderer();
            if (renderer instanceof JLabel) {
                if (parentLTR(c)) {
                    ins.left -= ((JLabel) renderer).getIconTextGap() - 1;
                } else {
                    ins.right -= ((JLabel) renderer).getIconTextGap() - 1;
                }
            }
        }
        return ins;
    }

    protected static boolean parentLTR(@NotNull final Component c) {
        return c.getParent().getComponentOrientation().isLeftToRight();
    }

    @Contract("null -> false")
    protected static boolean isListEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTextField.listCellEditor"))
                && c.getParent() instanceof JList;
    }

}
