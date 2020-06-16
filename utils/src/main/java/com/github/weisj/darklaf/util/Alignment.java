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
package com.github.weisj.darklaf.util;

import static com.github.weisj.darklaf.util.AlignmentHelper.*;

import java.awt.*;
import java.util.function.BiFunction;

/**
 * @author Jannis Weis
 */
public enum Alignment {
    NORTH(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_TOP_INSIDE),
            AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_TOP_OUTSIDE)),
    SOUTH(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_BOTTOM_INSIDE),
            AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_BOTTOM_OUTSIDE)),
    EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_CENTER_INSIDE),
            AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_CENTER_OUTSIDE)),
    WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_CENTER_INSIDE),
            AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_CENTER_OUTSIDE)),
    NORTH_EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_TOP_INSIDE),
            AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_TOP_OUTSIDE)),
    NORTH_WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_TOP_INSIDE),
            AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_TOP_OUTSIDE)),
    SOUTH_EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_BOTTOM_INSIDE),
            AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_BOTTOM_OUTSIDE)),
    SOUTH_WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_BOTTOM_INSIDE),
            AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_BOTTOM_OUTSIDE)),
    CENTER(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_CENTER_INSIDE),
            AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_CENTER_OUTSIDE));

    private final BiFunction<Dimension, Rectangle, Point> alignInside;
    private final BiFunction<Dimension, Rectangle, Point> alignOutside;

    Alignment(final BiFunction<Dimension, Rectangle, Point> alignInside,
              final BiFunction<Dimension, Rectangle, Point> alignOutside) {
        this.alignInside = alignInside;
        this.alignOutside = alignOutside;
    }

    /**
     * Get fitting alignment.
     *
     * @param  point       point to align at.
     * @param  size        Size of rectangle to align.
     * @param  outerBounds outer boundaries to align in.
     * @param  hint        preferred alignment.
     * @return             fitting alignment. If none is found the default is {@link Alignment#CENTER}.
     */
    public static Alignment getAlignment(final Point point,
                                         final Dimension size,
                                         final Rectangle outerBounds,
                                         final Alignment hint) {
        if (hint.canBeAligned(point, size, outerBounds)) {
            return hint;
        }
        for (Alignment alignment : Alignment.values()) {
            if (alignment != CENTER && alignment != hint
                && alignment.canBeAligned(point, size, outerBounds)) {
                return alignment;
            }
        }
        return CENTER;
    }

    /**
     * Check whether the given Rectangle can be aligned at point inside boundaries.
     *
     * @param  point       point to align at.
     * @param  size        size of rectangle to align.
     * @param  outerBounds boundaries.
     * @return             true if can be aligned.
     */
    public boolean canBeAligned(final Point point,
                                final Dimension size,
                                final Rectangle outerBounds) {
        Point p = relativePos(size, point);
        return p.x >= outerBounds.x && p.y >= outerBounds.y
               && p.x + size.width < outerBounds.x + outerBounds.width
               && p.y + size.height < outerBounds.x + outerBounds.height;
    }

    /**
     * Get the relative Position of Rectangle to Point with respect to the alignment.
     *
     * @param  toAlign size of Rectangle to align.
     * @param  alignAt point to align at.
     * @return         top/left position of aligned rectangle
     */
    public Point relativePos(final Dimension toAlign, final Point alignAt) {
        return alignOutside(toAlign, new Rectangle(alignAt.x, alignAt.y, 0, 0));
    }

    /**
     * Align Rectangle outside other rectangle with respect to the alignment.
     *
     * @param  toAlign     size of rectangle to align
     * @param  innerBounds bounds of inside rectangle
     * @return             top/left point of aligned rectangle
     */
    public Point alignOutside(final Dimension toAlign,
                              final Rectangle innerBounds) {
        return this.alignOutside.apply(toAlign, innerBounds);
    }

    /**
     * Get the index of the alignment. This function is for utility purposes where one might save settings based on
     * alignment in an array.
     *
     * @return the index.
     */
    public int getIndex() {
        return this.ordinal();
    }

    /**
     * Get the opposite alignment.
     *
     * @return Alignment opposite on the compass.
     */
    @SuppressWarnings("Duplicates")
    public Alignment opposite() {
        switch (this) {
            case NORTH :
                return SOUTH;
            case NORTH_EAST :
                return SOUTH_WEST;
            case EAST :
                return WEST;
            case SOUTH_EAST :
                return NORTH_WEST;
            case SOUTH :
                return NORTH;
            case SOUTH_WEST :
                return NORTH_EAST;
            case WEST :
                return EAST;
            case NORTH_WEST :
                return SOUTH_EAST;
            case CENTER :
                return CENTER;
            default :
                throw new IllegalArgumentException();
        }
    }

    @SuppressWarnings("Duplicates")
    public Alignment anticlockwise() {
        switch (this) {
            case NORTH :
                return NORTH_WEST;
            case NORTH_EAST :
                return NORTH;
            case EAST :
                return NORTH_EAST;
            case SOUTH_EAST :
                return EAST;
            case SOUTH :
                return SOUTH_EAST;
            case SOUTH_WEST :
                return SOUTH;
            case WEST :
                return SOUTH_WEST;
            case NORTH_WEST :
                return WEST;
            case CENTER :
                return CENTER;
            default :
                throw new IllegalArgumentException();
        }
    }

    @SuppressWarnings("Duplicates")
    public Alignment clockwise() {
        switch (this) {
            case NORTH :
                return NORTH_EAST;
            case NORTH_EAST :
                return EAST;
            case EAST :
                return SOUTH_EAST;
            case SOUTH_EAST :
                return SOUTH;
            case SOUTH :
                return SOUTH_WEST;
            case SOUTH_WEST :
                return WEST;
            case WEST :
                return NORTH_WEST;
            case NORTH_WEST :
                return NORTH;
            case CENTER :
                return CENTER;
            default :
                throw new IllegalArgumentException();
        }
    }

    public Insets maskInsets(final Insets insets) {
        return maskInsets(insets, 0);
    }

    public Insets maskInsets(final Insets insets, final int maskValue) {
        return maskInsets(insets.top, insets.left, insets.bottom, insets.right, maskValue);
    }

    public Insets maskInsets(final int top, final int left, final int bottom, final int right, final int mask) {
        switch (this) {
            case NORTH :
                return new Insets(top, mask, mask, mask);
            case NORTH_EAST :
                return new Insets(top, mask, mask, right);
            case EAST :
                return new Insets(mask, mask, mask, right);
            case SOUTH_EAST :
                return new Insets(mask, mask, bottom, right);
            case SOUTH :
                return new Insets(mask, mask, bottom, mask);
            case SOUTH_WEST :
                return new Insets(mask, left, bottom, mask);
            case WEST :
                return new Insets(mask, left, mask, mask);
            case NORTH_WEST :
                return new Insets(top, left, mask, mask);
            case CENTER :
                return new Insets(mask, mask, mask, mask);
            default :
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
        return new Insets(top * masking.top + maskVal.top,
                          left * masking.left + maskVal.left,
                          bottom * masking.bottom + maskVal.bottom,
                          right * masking.right + maskVal.right);
    }

    /**
     * Align Rectangle inside other rectangle with respect to the alignment.
     *
     * @param  toAlign     size of rectangle to align
     * @param  outerBounds bounds of outer rectangle
     * @return             top/left point of aligned rectangle
     */
    public Point alignInside(final Dimension toAlign,
                             final Rectangle outerBounds) {
        return this.alignInside.apply(toAlign, outerBounds);
    }

    public boolean isNorth() {
        return this == Alignment.NORTH || this == Alignment.NORTH_EAST || this == Alignment.NORTH_WEST;
    }

    public boolean isSouth() {
        return this == Alignment.SOUTH || this == Alignment.SOUTH_EAST || this == Alignment.SOUTH_WEST;
    }

    public boolean isEast() {
        return isEast(true);
    }

    public boolean isWest() {
        return isWest(true);
    }

    public boolean isEast(final boolean includePure) {
        return (this == Alignment.EAST && includePure) || this == Alignment.NORTH_EAST || this == Alignment.SOUTH_EAST;
    }

    public boolean isWest(final boolean includePure) {
        return (this == Alignment.WEST && includePure) || this == Alignment.NORTH_WEST || this == Alignment.SOUTH_WEST;
    }

    public boolean isVertical() {
        return this == Alignment.NORTH || this == Alignment.SOUTH;
    }

    public boolean isHorizontal() {
        return this == Alignment.EAST || this == Alignment.WEST;
    }

    public boolean isDiagonal() {
        return !isVertical() && !isHorizontal();
    }

    public double getAngle() {
        double angle = 0.0;
        switch (this) {
            case NORTH :
            case CENTER :
                angle = 0.0;
                break;
            case SOUTH :
                angle = 180.0;
                break;
            case EAST :
                angle = 90.0;
                break;
            case WEST :
                angle = 270.0;
                break;
            case NORTH_EAST :
                angle = 45.0;
                break;
            case NORTH_WEST :
                angle = 315.0;
                break;
            case SOUTH_EAST :
                angle = 135.0;
                break;
            case SOUTH_WEST :
                angle = 225.0;
                break;
        }
        return Math.toRadians(angle);
    }
}
