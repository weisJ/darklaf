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
package com.github.weisj.darklaf.ui.splitpane;

import java.awt.*;

import javax.swing.*;


public class DarkSplitPaneDividerPainter {

    protected final Icon verticalSplit;
    protected final Icon horizontalSplit;
    protected final Color borderColor;
    private final DarkSplitPaneUI ui;

    public DarkSplitPaneDividerPainter(final DarkSplitPaneUI ui) {
        this.ui = ui;
        verticalSplit = UIManager.getIcon("SplitPane.verticalGlue.icon");
        horizontalSplit = UIManager.getIcon("SplitPane.horizontalGlue.icon");
        borderColor = UIManager.getColor("SplitPane.dividerLineColor");
    }

    public void paint(final Component c, final Graphics g, final int x, final int y, final int w, final int h) {
        JSplitPane splitPane = ui.getSplitPane();
        if (splitPane == null) return;
        boolean paintBorder = ui.getStyle().isPaintBorder();
        if (splitPane.getOrientation() == JSplitPane.VERTICAL_SPLIT) {
            Icon icon = getVerticalSplitIcon();
            icon.paintIcon(c, g, x + (w - icon.getIconWidth()) / 2, y + (h - icon.getIconHeight()) / 2);
            if (paintBorder) {
                g.setColor(borderColor);
                g.fillRect(x, y, w, 1);
                g.fillRect(x, y + h - 1, w, 1);
            }
        } else {
            Icon icon = getHorizontalSplitIcon();
            icon.paintIcon(c, g, x + (w - icon.getIconWidth()) / 2, y + (h - icon.getIconHeight()) / 2);
            if (paintBorder) {
                g.setColor(borderColor);
                g.fillRect(x, y, 1, h);
                g.fillRect(x + w - 1, y, 1, h);
            }
        }
    }

    protected Icon getVerticalSplitIcon() {
        return verticalSplit;
    }

    protected Icon getHorizontalSplitIcon() {
        return horizontalSplit;
    }
}
