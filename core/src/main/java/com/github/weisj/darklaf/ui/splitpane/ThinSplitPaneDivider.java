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
package com.github.weisj.darklaf.ui.splitpane;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

final class ThinSplitPaneDivider extends BasicSplitPaneDivider {

    private final DarkSplitPaneUI ui;

    ThinSplitPaneDivider(final DarkSplitPaneUI ui) {
        super(ui);
        this.ui = ui;
    }

    @Override
    public int getDividerSize() {
        return ui.getStyle().isPaintBorder() ? 1 : 0;
    }

    @Override
    public void paint(final Graphics g) {
        if (ui.getStyle().isPaintBorder()) {
            g.setColor(ui.getDividerLineColor());
            int offset = ui.getDividerDragOffset();
            if (orientation == JSplitPane.HORIZONTAL_SPLIT) {
                g.drawLine(offset, 0, offset, getHeight());
            } else {
                g.drawLine(0, offset, getWidth(), offset);
            }
        }
    }

    @Override
    public boolean contains(final Point p) {
        if (!isEnabled()) return false;
        return super.contains(p);
    }

    @Override
    protected void dragDividerTo(final int location) {
        super.dragDividerTo(location + ui.getDividerDragOffset());
    }

    @Override
    protected void finishDraggingTo(final int location) {
        super.finishDraggingTo(location + ui.getDividerDragOffset());
    }
}
