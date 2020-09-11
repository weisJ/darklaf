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
package com.github.weisj.darklaf.components.uiresource;

public final class Insets2D implements Cloneable {

    public double top;
    public double left;
    public double bottom;
    public double right;

    /**
     * Creates and initializes a new <code>Insets</code> object with the specified top, left, bottom,
     * and right insets.
     *
     * @param top    the inset from the top.
     * @param left   the inset from the left.
     * @param bottom the inset from the bottom.
     * @param right  the inset from the right.
     */
    public Insets2D(final double top, final double left, final double bottom, final double right) {
        this.top = top;
        this.left = left;
        this.bottom = bottom;
        this.right = right;
    }

    /**
     * Set top, left, bottom, and right to the specified values
     *
     * @param top    the inset from the top.
     * @param left   the inset from the left.
     * @param bottom the inset from the bottom.
     * @param right  the inset from the right.
     * @since        1.5
     */
    public void set(final double top, final double left, final double bottom, final double right) {
        this.top = top;
        this.left = left;
        this.bottom = bottom;
        this.right = right;
    }

    @Override
    public int hashCode() {
        double sum1 = left + bottom;
        double sum2 = right + top;
        double val1 = sum1 * (sum1 + 1) / 2 + left;
        double val2 = sum2 * (sum2 + 1) / 2 + top;
        double sum3 = val1 + val2;
        return (int) (sum3 * (sum3 + 1) / 2 + val2);
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof Insets2D) {
            Insets2D insets = (Insets2D) obj;
            return ((top == insets.top) && (left == insets.left) && (bottom == insets.bottom)
                && (right == insets.right));
        }
        return false;
    }

    @Override
    public Insets2D clone() {
        return new Insets2D(top, left, bottom, right);
    }

    public String toString() {
        return getClass().getName() + "[top=" + top + ",left=" + left + ",bottom=" + bottom + ",right=" + right + "]";
    }
}
