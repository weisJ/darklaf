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
package com.github.weisj.darklaf.components.border;

import java.awt.*;

import javax.swing.border.AbstractBorder;

/** @author Jannis Weis */
public class MutableLineBorder extends AbstractBorder {

    private Color color;
    protected int top;
    protected int left;
    protected int right;
    protected int bottom;

    public MutableLineBorder(final Insets insets, final Color color) {
        this(insets.top, insets.left, insets.bottom, insets.right, color);
    }

    public MutableLineBorder(final int top, final int left, final int bottom, final int right, final Color color) {
        this.top = top;
        this.left = left;
        this.bottom = bottom;
        this.right = right;
        this.color = color;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        g.setColor(getColor());
        g.fillRect(x, y, width - getRight(), getTop());
        g.fillRect(x, y + getTop(), getLeft(), height - getTop());
        g.fillRect(x + getLeft(), y + height - getBottom(), width - getLeft(), getBottom());
        g.fillRect(x + width - getRight(), y, getRight(), height - getBottom());
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    public Color getColor() {
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

    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        insets.left = getLeft();
        insets.top = getTop();
        insets.right = getRight();
        insets.bottom = getBottom();
        return insets;
    }

    public Insets getBorderInsets() {
        return getBorderInsets(null, new Insets(0, 0, 0, 0));
    }

    public int getTop() {
        return top;
    }

    public int getBottom() {
        return bottom;
    }

    public int getLeft() {
        return left;
    }

    public int getRight() {
        return right;
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
