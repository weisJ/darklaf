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
package com.github.weisj.darklaf.color;

import java.awt.*;

import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.util.ColorWrapper;

/** @author Jannis Weis */
public final class ColorUtil {

    public static Color blendColors(final Color color1, final Color color2, final double percent) {
        if (percent == 1) return color1;
        if (percent == 0) return color2;
        double inverse_percent = 1.0 - percent;
        int redPart = (int) Math.round(color1.getRed() * percent + color2.getRed() * inverse_percent);
        int greenPart = (int) Math.round(color1.getGreen() * percent + color2.getGreen() * inverse_percent);
        int bluePart = (int) Math.round(color1.getBlue() * percent + color2.getBlue() * inverse_percent);
        return new Color(redPart, greenPart, bluePart);
    }

    public static Color shift(final Color c, final double d) {
        return new Color(shift(c.getRed(), d), shift(c.getGreen(), d), shift(c.getBlue(), d), c.getAlpha());
    }

    private static int shift(final int colorComponent, final double d) {
        int n = (int) Math.round((double) colorComponent * d);
        return n > 255 ? 255 : (Math.max(n, 0));
    }

    public static Color toAlpha(final Color color, final double alpha) {
        return toAlpha(color, (int) Math.round(alpha * 255));
    }

    public static Color toAlpha(final Color color, final int a) {
        Color c = color != null ? color : Color.black;
        return new Color(c.getRed(), c.getGreen(), c.getBlue(), Math.min(Math.max(0, a), 255));
    }

    public static Color fromHex(final String str, final Color defaultValue) {
        try {
            return fromHex(str);
        } catch (final Exception var3) {
            return defaultValue;
        }
    }

    public static String toHex(final Color color) {
        if (color == null) return "";
        int r = color.getRed();
        int b = color.getBlue();
        int g = color.getGreen();
        return String.format("%02X%02X%02X", r, g, b);
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

    public static boolean canOverwriteColor(final Color c) {
        return c == null || c instanceof UIResource || c instanceof NonUIResourceOverwritableColorWrapper;
    }

    public static Color stripUIResource(final Color c, final boolean canOverwrite) {
        if (c instanceof UIResource) {
            if (canOverwrite) {
                return new NonUIResourceOverwritableColorWrapper(c);
            } else {
                return new NonUIResourceColorWrapper(c);
            }
        } else {
            return c;
        }
    }

    public static Color removeAlpha(final Color color) {
        return toAlpha(color, 255);
    }

    /**
     * Calculate the perceived brightness of a color.
     *
     * @param c the color.
     * @return the brightness from 0 to 255.
     */
    public static double getPerceivedBrightness(final Color c) {
        return Math.sqrt(0.299 * c.getRed() * c.getRed() + 0.587 * c.getGreen() * c.getGreen()
                + 0.114 * c.getBlue() * c.getBlue());
    }

    public static double getLuminance(final Color c) {
        return getLuminance(c.getRed(), c.getGreen(), c.getBlue());
    }

    public static double getLuminance(final int red, final int green, final int blue) {
        double r = red / 255.0;
        double g = green / 255.0;
        double b = blue / 255.0;
        double R = r <= 0.03928 ? r / 12.92 : Math.pow((r + 0.055) / 1.055, 2.4);
        double G = g <= 0.03928 ? g / 12.92 : Math.pow((g + 0.055) / 1.055, 2.4);
        double B = b <= 0.03928 ? b / 12.92 : Math.pow((b + 0.055) / 1.055, 2.4);
        return 0.2126 * R + 0.7152 * G + 0.0722 * B;
    }

    public static class NonUIResourceColorWrapper extends ColorWrapper {

        public NonUIResourceColorWrapper(final Color color) {
            super(color);
        }
    }

    public static class NonUIResourceOverwritableColorWrapper extends NonUIResourceColorWrapper {

        public NonUIResourceOverwritableColorWrapper(final Color color) {
            super(color);
        }
    }
}
