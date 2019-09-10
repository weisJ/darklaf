package com.weis.darklaf.components.alignment;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.function.BiFunction;

/**
 * Helper methods for calculating alignments.
 *
 * @author Jannis Weis
 * @since 2018
 */
final class AlignmentHelper {

    /**
     * Provided relative mapping functions.
     */
    /*default*/ static final Mapper HOR_CENTER_INSIDE = (d, r) -> r.x + (r.width - d.width) / 2;
    /*default*/ static final Mapper HOR_LEFT_INSIDE = (d, r) -> r.x;
    /*default*/ static final Mapper HOR_RIGHT_INSIDE = (d, r) -> r.x + r.width - d.width;
    /*default*/ static final Mapper VERT_CENTER_INSIDE = (d, r) -> r.y + (r.height - d.height) / 2;
    /*default*/ static final Mapper VERT_TOP_INSIDE = (d, r) -> r.y;
    /*default*/ static final Mapper VERT_BOTTOM_INSIDE = (d, r) -> r.y + r.height - d.height;

    /*default*/ static final Mapper HOR_CENTER_OUTSIDE = HOR_CENTER_INSIDE;
    /*default*/ static final Mapper HOR_LEFT_OUTSIDE = (d, r) -> r.x - d.width;
    /*default*/ static final Mapper HOR_RIGHT_OUTSIDE = (d, r) -> r.x;
    /*default*/ static final Mapper VERT_CENTER_OUTSIDE = VERT_CENTER_INSIDE;
    /*default*/ static final Mapper VERT_TOP_OUTSIDE = (d, r) -> r.y - d.height;
    /*default*/ static final Mapper VERT_BOTTOM_OUTSIDE = (d, r) -> r.y + r.height;

    /**
     * Create mapper from component mapper.
     *
     * @param mapperX x component mapper.
     * @param mapperY y component mapper.
     * @return mapper that aligns a rectangle relative to other rectangle.
     */
    @NotNull
    @Contract(pure = true)
    /*default*/ static BiFunction<Dimension, Rectangle, Point> align(
            @NotNull final Mapper mapperX, @NotNull final Mapper mapperY) {
        return (d, p) -> new Point(mapperX.apply(d, p), mapperY.apply(d, p));
    }

    /**
     * Helper interface to avoid long type names.
     */
    private interface Mapper extends BiFunction<Dimension, Rectangle, Integer> {
    }
}
