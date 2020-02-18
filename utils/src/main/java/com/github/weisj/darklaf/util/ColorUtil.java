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

/**
 * @author Jannis Weis
 */
public final class ColorUtil {


    private ColorUtil() {

    }


    public static Color shift(final Color c, final double d) {
        return new Color(shift(c.getRed(), d), shift(c.getGreen(), d), shift(c.getBlue(), d), c.getAlpha());
    }


    private static int shift(final int colorComponent, final double d) {
        int n = (int) ((double) colorComponent * d);
        return n > 255 ? 255 : (Math.max(n, 0));
    }


    public static Color toAlpha(final Color color, final double alpha) {
        return toAlpha(color, (int) (alpha * 255));
    }


    public static Color toAlpha(final Color color, final int a) {
        Color c = color != null ? color : Color.black;
        return new Color(c.getRed(), c.getGreen(), c.getBlue(), Math.min(Math.max(0, a), 255));
    }

    public static Color fromHex(final String str, final Color defaultValue) {
        try {
            return fromHex(str);
        } catch (Exception var3) {
            return defaultValue;
        }
    }


    public static Color fromHex(String str) {
        if (str.startsWith("#")) {
            str = str.substring(1);
        }

        if (str.length() == 3) {
            return new Color(17 * Integer.valueOf(String.valueOf(str.charAt(0)), 16),
                             17 * Integer.valueOf(String.valueOf(str.charAt(1)), 16),
                             17 * Integer.valueOf(String.valueOf(str.charAt(2)), 16));
        } else if (str.length() == 6) {
            return Color.decode("0x" + str);
        } else {
            throw new IllegalArgumentException("Should be String of 3 or 6 chars length.");
        }
    }

    public static Color removeAlpha(final Color color) {
        return toAlpha(color, 255);
    }
}
