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
package com.github.weisj.darklaf.ui;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.cell.CellUtil;

public final class DividedWidgetPainter {

    public static void paintBackground(final Graphics2D g, final JComponent c, final int arc,
            final Rectangle splitBounds, final Color bg, final Color splitBg, final boolean isCellEditor) {
        int w = c.getWidth();
        int h = c.getHeight();
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        Insets ins = c.getInsets();

        g.setColor(bg);
        fillBackground(g, w, h, arc, ins, isCellEditor);

        if (splitBounds != null) {
            Shape oldClip = g.getClip();
            if (ltr) {
                g.clipRect(splitBounds.x, 0, w - splitBounds.x, h);
            } else {
                g.clipRect(0, 0, splitBounds.x + splitBounds.width, h);
            }

            g.setColor(splitBg);
            fillBackground(g, w, h, arc, ins, isCellEditor);
            g.setClip(oldClip);
        }
    }

    private static void fillBackground(final Graphics2D g, final int w, final int h, final int arc, final Insets ins,
            final boolean isCellEditor) {
        if (!isCellEditor) {
            PaintUtil.fillRoundRect(g, w, h, ins, arc);
        } else {
            g.fillRect(ins.left, ins.top, w - ins.right - ins.left, h - ins.top - ins.bottom);
        }
    }

    public static void paintBorder(final Graphics2D g, final JComponent c, final int width, final int height,
            final int arc, final int borderSize, final int dividerLocation, final boolean isTableEditor,
            final boolean isTreeEditor, final boolean hasFocus, final Color borderColor, final Color focusBorderColor) {
        boolean isCellEditor = isTableEditor || isTreeEditor;
        Insets ins = c.getInsets();

        g.setColor(borderColor);
        if (dividerLocation >= 0) {
            g.fillRect(dividerLocation, ins.top, 1, height - ins.top - ins.bottom);
        }
        if (!isCellEditor) {
            if (hasFocus) {
                PaintUtil.paintFocusBorder(g, width, height, arc, borderSize);
                g.setColor(focusBorderColor);
            }
            PaintUtil.paintLineBorder(g, width, height, ins, arc);
        } else {
            paintCellBorder(g, c, width, height, ins, isTableEditor);
        }
    }

    private static void paintCellBorder(final Graphics2D g, final Component c, final int width, final int height,
            final Insets ins, final boolean isTableCellEditor) {
        Component parent = c.getParent();
        if (isTableCellEditor && parent instanceof JTable) {
            JTable table = ((JTable) parent);
            CellUtil.paintTableEditorBorder(g, c, table, width, height);
        } else {
            PaintUtil.drawRect(g, ins.left, ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom, 1);
        }
    }
}
