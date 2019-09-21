package com.weis.darklaf.components.alignment;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.function.BiFunction;

import static com.weis.darklaf.components.alignment.AlignmentHelper.*;

/**
 * Alignment for GUI elements.
 *
 * @author Jannis Weis
 * @since 2018
 */
public enum Alignment {
    NORTH(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_TOP_INSIDE),
          AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_TOP_OUTSIDE), 0
    ),
    SOUTH(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_BOTTOM_INSIDE),
          AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_BOTTOM_OUTSIDE), 1
    ),
    EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_CENTER_INSIDE),
         AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_CENTER_OUTSIDE), 2
    ),
    WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_CENTER_INSIDE),
         AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_CENTER_OUTSIDE), 3
    ),
    NORTH_EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_TOP_INSIDE),
               AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_TOP_OUTSIDE), 4
    ),
    NORTH_WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_TOP_INSIDE),
               AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_TOP_OUTSIDE), 5
    ),
    SOUTH_EAST(AlignmentHelper.align(HOR_RIGHT_INSIDE, VERT_BOTTOM_INSIDE),
               AlignmentHelper.align(HOR_RIGHT_OUTSIDE, VERT_BOTTOM_OUTSIDE), 6
    ),
    SOUTH_WEST(AlignmentHelper.align(HOR_LEFT_INSIDE, VERT_BOTTOM_INSIDE),
               AlignmentHelper.align(HOR_LEFT_OUTSIDE, VERT_BOTTOM_OUTSIDE), 7
    ),
    CENTER(AlignmentHelper.align(HOR_CENTER_INSIDE, VERT_CENTER_INSIDE),
           AlignmentHelper.align(HOR_CENTER_OUTSIDE, VERT_CENTER_OUTSIDE), 8
    );


    private final BiFunction<Dimension, Rectangle, Point> alignInside;
    private final BiFunction<Dimension, Rectangle, Point> alignOutside;
    private final int index;

    @Contract(pure = true)
    Alignment(final BiFunction<Dimension, Rectangle, Point> alignInside,
              final BiFunction<Dimension, Rectangle, Point> alignOutside,
              final int index) {
        this.index = index;
        this.alignInside = alignInside;
        this.alignOutside = alignOutside;
    }

    /**
     * Get fitting alignment.
     *
     * @param point       point to align at.
     * @param size        Size of rectangle to align.
     * @param outerBounds outer boundaries to align in.
     * @param hint        preferred alignment.
     * @return fitting alignment. If none is found wit is defaulted to {@link Alignment#CENTER}.
     */
    @NotNull
    public static Alignment getAlignment(@NotNull final Point point,
                                         @NotNull final Dimension size,
                                         @NotNull final Rectangle outerBounds,
                                         @NotNull final Alignment hint) {
        if (hint.canBeAligned(point, size, outerBounds)) {
            return hint;
        }
        for (var alignment : Alignment.values()) {
            if (alignment != CENTER && alignment != hint
                && alignment.canBeAligned(point, size, outerBounds)) {
                return alignment;
            }
        }
        return CENTER;
    }

    /**
     * Get the index of the alignment. This function is for utility purposes where one might save
     * settings based on alignment in an array.
     *
     * @return the index.
     */
    @Contract(pure = true)
    public int getIndex() {
        return index;
    }

    /**
     * Get the opposite alignment.
     *
     * @return Alignment opposite on the compass.
     */
    @Contract(pure = true)
    @NotNull
    @SuppressWarnings("Duplicates")
    public Alignment opposite() {
        switch (this) {
            case NORTH:
                return SOUTH;
            case NORTH_EAST:
                return SOUTH_WEST;
            case EAST:
                return WEST;
            case SOUTH_EAST:
                return NORTH_WEST;
            case SOUTH:
                return NORTH;
            case SOUTH_WEST:
                return NORTH_EAST;
            case WEST:
                return EAST;
            case NORTH_WEST:
                return SOUTH_EAST;
            case CENTER:
                return CENTER;
            default:
                throw new IllegalArgumentException();
        }
    }

    @NotNull
    @Contract(pure = true)
    @SuppressWarnings("Duplicates")
    public Alignment anticlockwise() {
        switch (this) {
            case NORTH:
                return NORTH_WEST;
            case NORTH_EAST:
                return NORTH;
            case EAST:
                return NORTH_EAST;
            case SOUTH_EAST:
                return EAST;
            case SOUTH:
                return SOUTH_EAST;
            case SOUTH_WEST:
                return SOUTH;
            case WEST:
                return SOUTH_WEST;
            case NORTH_WEST:
                return WEST;
            case CENTER:
                return CENTER;
            default:
                throw new IllegalArgumentException();
        }
    }

    @NotNull
    @Contract(pure = true)
    @SuppressWarnings("Duplicates")
    public Alignment clockwise() {
        switch (this) {
            case NORTH:
                return NORTH_EAST;
            case NORTH_EAST:
                return EAST;
            case EAST:
                return SOUTH_EAST;
            case SOUTH_EAST:
                return SOUTH;
            case SOUTH:
                return SOUTH_WEST;
            case SOUTH_WEST:
                return WEST;
            case WEST:
                return NORTH_WEST;
            case NORTH_WEST:
                return NORTH;
            case CENTER:
                return CENTER;
            default:
                throw new IllegalArgumentException();
        }
    }

    @NotNull
    @Contract(pure = true)
    public Insets maskInsets(@NotNull final Insets insets) {
        switch (this) {
            case NORTH:
                return new Insets(insets.top, 0, 0, 0);
            case NORTH_EAST:
                return new Insets(insets.top, 0, 0, insets.right);
            case EAST:
                return new Insets(0, 0, 0, insets.right);
            case SOUTH_EAST:
                return new Insets(0, 0, insets.bottom, insets.right);
            case SOUTH:
                return new Insets(0, 0, insets.bottom, 0);
            case SOUTH_WEST:
                return new Insets(0, insets.left, insets.bottom, 0);
            case WEST:
                return new Insets(0, insets.left, 0, 0);
            case NORTH_WEST:
                return new Insets(insets.top, insets.left, 0, 0);
            case CENTER:
                return new Insets(0, 0, 0, 0);
            default:
                throw new IllegalArgumentException();
        }
    }

    /**
     * Get the relative Position of Rectangle to Point with respect to the alignment.
     *
     * @param toAlign size of Rectangle to align.
     * @param alignAt point to align at.
     * @return top/left position of aligned rectangle
     */
    public Point relativePos(@NotNull final Dimension toAlign, @NotNull final Point alignAt) {
        return alignOutside(toAlign, new Rectangle(alignAt.x, alignAt.y, 0, 0));
    }


    /**
     * Check whether the given Rectangle can be aligned at point inside boundaries.
     *
     * @param point       point to align at.
     * @param size        size of rectangle to align.
     * @param outerBounds boundaries.
     * @return true if can be aligned.
     */
    public boolean canBeAligned(@NotNull final Point point,
                                @NotNull final Dimension size,
                                @NotNull final Rectangle outerBounds) {
        var p = relativePos(size, point);
        return p.x >= outerBounds.x && p.y >= outerBounds.y
               && p.x + size.width < outerBounds.x + outerBounds.width
               && p.y + size.height < outerBounds.x + outerBounds.height;
    }

    /**
     * Align Rectangle inside other rectangle with respect to the alignment.
     *
     * @param toAlign     size of rectangle to align
     * @param outerBounds bounds of outer rectangle
     * @return top/left point of aligned rectangle
     */
    public Point alignInside(@NotNull final Dimension toAlign,
                             @NotNull final Rectangle outerBounds) {
        return this.alignInside.apply(toAlign, outerBounds);
    }

    /**
     * Align Rectangle outside other rectangle with respect to the alignment.
     *
     * @param toAlign     size of rectangle to align
     * @param innerBounds bounds of inside rectangle
     * @return top/left point of aligned rectangle
     */
    public Point alignOutside(@NotNull final Dimension toAlign,
                              @NotNull final Rectangle innerBounds) {
        return this.alignOutside.apply(toAlign, innerBounds);
    }
}
