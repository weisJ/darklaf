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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.util.logging.Logger;

public final class Scale {
    public static final double SCALE;
    public static final double SCALE_X;
    public static final double SCALE_Y;
    private static final Logger LOGGER = Logger.getLogger(Scale.class.getName());

    static {
        DisplayMode mode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        SCALE_X = mode.getWidth() / (double) screenSize.width;
        SCALE_Y = mode.getHeight() / (double) screenSize.height;
        SCALE = SCALE_X;
        LOGGER.info("Using screen scaling SCALE_X=" + SCALE_X + ", SCALE_Y=" + SCALE_Y);
    }


    public static int scale(final int i) {
        return (int) (SCALE * i);
    }


    public static float scale(final float f) {
        return (float) (SCALE * f);
    }


    public static double scale(final double d) {
        return SCALE * d;
    }


    public static int scaleWidth(final int i) {
        return (int) (SCALE_X * i);
    }


    public static float scaleWidth(final float f) {
        return (float) (SCALE_X * f);
    }


    public static double scaleWidth(final double d) {
        return SCALE_X * d;
    }


    public static int scaleHeight(final int i) {
        return (int) (SCALE_Y * i);
    }


    public static float scaleHeight(final float f) {
        return (float) (SCALE_Y * f);
    }


    public static double scaleHeight(final double d) {
        return SCALE_Y * d;
    }
}
