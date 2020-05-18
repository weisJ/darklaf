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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;

public class DarkMacScrollBarUI extends DarkScrollBarUI {

    public static ComponentUI createUI(final JComponent c) {
        return new DarkMacScrollBarUI();
    }

    @Override
    protected void paintMaxiThumb(final Graphics2D g, final Rectangle rect) {
        GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
        g.setComposite(COMPOSITE.derive(THUMB_ALPHA));
        g.setColor(getThumbColor());
        boolean horizontal = scrollbar.getOrientation() == JScrollBar.HORIZONTAL;
        int arc = horizontal ? (rect.height - 2) : (rect.width - 2);
        g.fillRoundRect(rect.x + 1, rect.y + 1, rect.width - 2, rect.height - 2, arc, arc);
        context.restore();
    }
}
