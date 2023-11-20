/*
 * MIT License
 *
 * Copyright (c) 2023 Jannis Weis
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
package com.github.weisj.darklaf.components.border;

import java.awt.*;

import javax.swing.border.Border;

import com.github.weisj.darklaf.graphics.PaintUtil;

public class RoundedLineBorder implements Border {

    private final Color color;
    private final int arc;
    private final int thickness;

    public RoundedLineBorder(final Color color, final int arc, final int thickness) {
        this.color = color;
        this.arc = arc;
        this.thickness = thickness;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        g.setColor(color);
        if (arc == 0) {
            PaintUtil.drawRect(g, new Rectangle(x, y, width, height), thickness);
        } else {
            ((Graphics2D) g).setStroke(new BasicStroke(thickness));
            PaintUtil.paintLineBorder((Graphics2D) g, x, y, width, height, arc);
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new Insets(thickness, thickness, thickness, thickness);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
