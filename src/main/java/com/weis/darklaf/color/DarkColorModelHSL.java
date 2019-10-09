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
package com.weis.darklaf.color;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkColorModelHSL extends DarkColorModel {

    private static final int[] hsl = new int[3];
    private static final int[] rgb = new int[3];

    public DarkColorModelHSL() {
        super("hsl", "Hue", "Saturation", "Lightness");
    }

    @Override
    public int getMaximum(final int index) {
        return (index == 0) ? 359 : 100;
    }

    @Override
    public String toString() {
        return "HSL";
    }

    @Override
    public char[] getLabelDescriptorsBefore() {
        return new char[]{'H', 'S', 'L'};
    }

    @Override
    public char[] getLabelDescriptorsAfter() {
        return new char[]{'\u00B0', '%', '%'};
    }

    @Override
    public int[] getValuesFromColor(@NotNull final Color color) {
        return RGBtoHSL(color.getRed(), color.getGreen(), color.getBlue());
    }

    @NotNull
    private static int[] RGBtoHSL(final int r, final int g, final int b) {
        double max = max(r, g, b) / 255.0;
        double min = min(r, g, b) / 255.0;

        double summa = (max + min);
        double saturation = (max - min);
        if (saturation > 0.0f) {
            saturation /= (summa > 1.0f)
                          ? 2.0f - summa
                          : summa;
        }
        hsl[0] = (int) Math.round(360 * getHue(r / 255.0, g / 255.0, b / 255.0, max, min));
        hsl[1] = (int) Math.round(100 * saturation);
        hsl[2] = (int) Math.round(100 * (summa / 2.0));
        return hsl;
    }

    @Contract(pure = true)
    protected static double max(final double red, final double green, final double blue) {
        double max = Math.max(red, green);
        return Math.max(max, blue);
    }

    protected static double min(final double red, final double green, final double blue) {
        double min = Math.min(red, green);
        return Math.min(min, blue);
    }

    @Contract(pure = true)
    private static double getHue(final double red, final double green, final double blue,
                                 final double max, final double min) {
        double hue = max - min;
        if (hue > 0.0f) {
            if (max == red) {
                hue = (green - blue) / hue;
                if (hue < 0.0f) {
                    hue += 6.0f;
                }
            } else if (max == green) {
                hue = 2.0f + (blue - red) / hue;
            } else /*max == blue*/ {
                hue = 4.0f + (red - green) / hue;
            }
            hue /= 6.0f;
        }
        return hue;
    }

    @Override
    public Color getColorFromValues(@NotNull final int[] values) {
        var rgb = HSLtoRGB(values[0] / 360.0, values[1] / 100.0, values[2] / 100.0);
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    @NotNull
    private static int[] HSLtoRGB(final double h, final double saturation, final double lightness) {
        double hue = h;

        if (saturation > 0.0f) {
            hue = (hue < 1.0f) ? hue * 6.0f : 0.0f;
            double q = lightness + saturation * ((lightness > 0.5f) ? 1.0f - lightness : lightness);
            double p = 2.0f * lightness - q;
            rgb[0] = (int) Math.round(255 * normalize(q, p, (hue < 4.0f) ? (hue + 2.0f) : (hue - 4.0f)));
            rgb[1] = (int) Math.round(255 * normalize(q, p, hue));
            rgb[2] = (int) Math.round(255 * normalize(q, p, (hue < 2.0f) ? (hue + 4.0f) : (hue - 2.0f)));
        } else {
            rgb[0] = (int) Math.round(255 * lightness);
            rgb[1] = rgb[0];
            rgb[2] = rgb[0];
        }
        return rgb;
    }

    @Contract(pure = true)
    private static double normalize(final double q, final double p, final double color) {
        if (color < 1.0f) {
            return p + (q - p) * color;
        }
        if (color < 3.0f) {
            return q;
        }
        if (color < 4.0f) {
            return p + (q - p) * (4.0f - color);
        }
        return p;
    }
}
