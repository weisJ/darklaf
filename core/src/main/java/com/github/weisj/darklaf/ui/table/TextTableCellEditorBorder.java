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
package com.github.weisj.darklaf.ui.table;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;

/**
 * @author Jannis Weis
 */
public class TextTableCellEditorBorder extends DarkTableCellBorder {

    protected final Color borderColor;

    public TextTableCellEditorBorder() {
        borderColor = UIManager.getColor("TextField.border.enabled");
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(borderColor);
        JTable table = getTable(c);
        if (table != null) {
            CellUtil.paintTableEditorBorder(g, c, table, width, height);
        } else {
            PaintUtil.drawRect(g, x, y, width, height, 1);
        }
    }

    private JTable getTable(final Component c) {
        return DarkUIUtil.getParentOfType(JTable.class, c, 2);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return CellUtil.adjustEditorInsets(super.getBorderInsets(), c, getTable(c));
    }
}
