/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.components.alignment;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.function.BiFunction;

/**
 * @author Jannis Weis
 */
final class AlignmentHelper {

    /**
     * Provided relative mapping functions.
     */
    static final Mapper HOR_CENTER_INSIDE = (d, r) -> r.x + (r.width - d.width) / 2;
    static final Mapper HOR_LEFT_INSIDE = (d, r) -> r.x;
    static final Mapper HOR_RIGHT_INSIDE = (d, r) -> r.x + r.width - d.width;
    static final Mapper VERT_CENTER_INSIDE = (d, r) -> r.y + (r.height - d.height) / 2;
    static final Mapper VERT_TOP_INSIDE = (d, r) -> r.y;
    static final Mapper VERT_BOTTOM_INSIDE = (d, r) -> r.y + r.height - d.height;

    static final Mapper HOR_CENTER_OUTSIDE = HOR_CENTER_INSIDE;
    static final Mapper HOR_LEFT_OUTSIDE = (d, r) -> r.x - d.width;
    static final Mapper HOR_RIGHT_OUTSIDE = (d, r) -> r.x;
    static final Mapper VERT_CENTER_OUTSIDE = VERT_CENTER_INSIDE;
    static final Mapper VERT_TOP_OUTSIDE = (d, r) -> r.y - d.height;
    static final Mapper VERT_BOTTOM_OUTSIDE = (d, r) -> r.y + r.height;

    /**
     * Create mapper from component mapper.
     *
     * @param mapperX x component mapper.
     * @param mapperY y component mapper.
     * @return mapper that aligns a rectangle relative to other rectangle.
     */
    @NotNull
    @Contract(pure = true)
    static BiFunction<Dimension, Rectangle, Point> align(
            @NotNull final Mapper mapperX, @NotNull final Mapper mapperY) {
        return (d, p) -> new Point(mapperX.apply(d, p), mapperY.apply(d, p));
    }

    /**
     * Helper interface to avoid long type names.
     */
    private interface Mapper extends BiFunction<Dimension, Rectangle, Integer> {
    }
}
