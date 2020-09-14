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
package com.github.weisj.darklaf.util;

import java.awt.*;

public enum AlignmentExt {
    NORTH(Alignment.NORTH), SOUTH(Alignment.SOUTH), EAST(Alignment.EAST), WEST(Alignment.WEST),
    NORTH_EAST(Alignment.NORTH_EAST), NORTH_WEST(Alignment.NORTH_WEST), SOUTH_EAST(Alignment.SOUTH_EAST),
    SOUTH_WEST(Alignment.SOUTH_WEST), CENTER(Alignment.CENTER), LEFT(null), MIDDLE_HORIZONTAL(null), RIGHT(null),
    TOP(null), MIDDLE_VERTICAL(null), BOTTOM(null);

    private final Alignment parent;

    AlignmentExt(final Alignment parent) {
        this.parent = parent;
    }

    public Insets maskInsets(final Insets insets) {
        return maskInsets(insets, 0);
    }

    public Insets maskInsets(final Insets insets, final int maskValue) {
        return maskInsets(insets.top, insets.left, insets.bottom, insets.right, maskValue);
    }

    public Insets maskInsets(final int top, final int left, final int bottom, final int right, final int mask) {
        switch (this) {
            case NORTH:
            case NORTH_EAST:
            case EAST:
            case SOUTH_EAST:
            case SOUTH:
            case SOUTH_WEST:
            case WEST:
            case NORTH_WEST:
            case CENTER:
                return parent.maskInsets(top, left, bottom, right, mask);
            case LEFT:
                return new Insets(top, left, bottom, mask);
            case MIDDLE_HORIZONTAL:
                return new Insets(top, mask, bottom, mask);
            case RIGHT:
                return new Insets(top, mask, bottom, right);
            case TOP:
                return new Insets(top, left, mask, right);
            case MIDDLE_VERTICAL:
                return new Insets(mask, left, mask, right);
            case BOTTOM:
                return new Insets(mask, left, bottom, right);
            default:
                throw new IllegalArgumentException();
        }
    }

    public Insets maskInsetsInverted(final Insets insets) {
        return maskInsetsInverted(insets, 0);
    }

    public Insets maskInsetsInverted(final Insets insets, final int mask) {
        return maskInsetsInverted(insets.top, insets.left, insets.bottom, insets.right, mask);
    }

    public Insets maskInsetsInverted(final int top, final int left, final int bottom, final int right, final int mask) {
        Insets masking = maskInsets(0, 0, 0, 0, 1);
        Insets maskVal = maskInsets(mask, mask, mask, mask, 0);
        return new Insets(top * masking.top + maskVal.top, left * masking.left + maskVal.left,
                bottom * masking.bottom + maskVal.bottom, right * masking.right + maskVal.right);
    }
}
