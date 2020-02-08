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
 */
package com.github.weisj.darklaf.components.border;

import org.jetbrains.annotations.NotNull;

import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class MutableLineBorder extends EmptyBorder {

    private Color color;

    public MutableLineBorder(
            final int top, final int left, final int bottom, final int right, final Color color) {
        super(top, left, bottom, right);
        this.color = color;
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(getColor());
        g.fillRect(x, y, width - right, top);
        g.fillRect(x, y + top, left, height - top);
        g.fillRect(x + left, y + height - bottom, width - left, bottom);
        g.fillRect(x + width - right, y, right, height - bottom);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    protected Color getColor() {
        return color;
    }

    public void setColor(final Color color) {
        this.color = color;
    }

    public void setInsets(final int top, final int left, final int bottom, final int right) {
        setTop(top);
        setBottom(bottom);
        setLeft(left);
        setRight(right);
    }

    public void setTop(final int top) {
        this.top = top;
    }

    public void setBottom(final int bottom) {
        this.bottom = bottom;
    }

    public void setLeft(final int left) {
        this.left = left;
    }

    public void setRight(final int right) {
        this.right = right;
    }

    public static class UIResource extends MutableLineBorder implements javax.swing.plaf.UIResource {

        public UIResource(final int top, final int left, final int bottom, final int right, final Color color) {
            super(top, left, bottom, right, color);
        }
    }
}
