/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.ui.table;

import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class TextFieldTableCellEditorBorder extends DarkTableCellBorder {

    protected Color borderColor;

    public TextFieldTableCellEditorBorder() {
        borderColor = UIManager.getColor("TextField.border.enabled");
    }

    @Override
    public void paintBorder(@NotNull final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(borderColor);
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
