/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.table;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellBorderUIResource;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkTableCellFocusBorder extends DarkCellBorderUIResource {

    protected final Color rowBorderColor;
    protected final Color borderColor;

    public DarkTableCellFocusBorder() {
        rowBorderColor = UIManager.getColor("Table.focusRowBorderColor");
        borderColor = UIManager.getColor("Table.focusBorderColor");
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        super.paintBorder(c, g, x, y, width, height);
        if (isRowFocusBorder(c)) {
            g.setColor(rowBorderColor);
            g.fillRect(0, 0, width, 1);
            g.fillRect(0, height - 1, width, 1);
            if (forcePaintLeft(c)) {
                g.fillRect(0, 0, 1, height);
            }
            if (forcePaintRight(c)) {
                g.fillRect(width - 1, 0, 1, height);
            }
        } else {
            g.setColor(borderColor);
            PaintUtil.drawRect(g, 0, 0, width, height, 1);
        }
    }

    public static boolean isRowFocusBorder(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTableUI.KEY_FULL_ROW_FOCUS_BORDER);
    }

    protected static boolean forcePaintLeft(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTableUI.KEY_FORCE_LEFT_BORDER);
    }

    protected static boolean forcePaintRight(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTableUI.KEY_FORCE_RIGHT_BORDER);
    }
}
