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
package com.github.weisj.darklaf.color;

import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkColorModelHSL extends DarkColorModel {

    private static final int[] hsl = new int[3];
    private static final int[] rgb = new int[3];

    private static DarkColorModelHSL instance;

    public static DarkColorModelHSL getInstance() {
        if (instance == null) instance = new DarkColorModelHSL();
        return instance;
    }

    public DarkColorModelHSL() {
        super("hsl", "Hue", "Saturation", "Lightness");
    }

    @Override
    public int getMinimum(final int index) {
        return 0;
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
    public int[] getValuesFromColor(final Color color) {
        return RGBtoHSL(color.getRed(), color.getGreen(), color.getBlue());
    }

    public static Color getColorFromHSLValues(final double h, final double s, final double l) {
        int[] rgb = HSLtoRGB(h, s, l);
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    public static double[] RGBtoHSLValues(final int r, final int g, final int b) {
        double max = max(r, g, b) / 255.0;
        double min = min(r, g, b) / 255.0;

        double summa = (max + min);
        double saturation = (max - min);
        if (saturation > 0.0f) {
            saturation /= (summa > 1.0f)
                                         ? 2.0f - summa
                                         : summa;
        }
        return new double[]{
                            getHue(r / 255.0, g / 255.0, b / 255.0, max, min),
                            saturation,
                            summa / 2.0
        };
    }

    private static int[] RGBtoHSL(final int r, final int g, final int b) {
        double[] values = RGBtoHSLValues(r, g, b);
        hsl[0] = (int) Math.round(360 * values[0]);
        hsl[1] = (int) Math.round(100 * values[1]);
        hsl[2] = (int) Math.round(100 * values[2]);
        return hsl;
    }

    protected static double max(final double red, final double green, final double blue) {
        double max = Math.max(red, green);
        return Math.max(max, blue);
    }

    protected static double min(final double red, final double green, final double blue) {
        double min = Math.min(red, green);
        return Math.min(min, blue);
    }

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
            } else /* max == blue */ {
                hue = 4.0f + (red - green) / hue;
            }
            hue /= 6.0f;
        }
        return hue;
    }

    @Override
    public Color getColorFromValues(final int[] values) {
        int[] rgb = HSLtoRGB(values[0] / 360.0, values[1] / 100.0, values[2] / 100.0);
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    private static int[] HSLtoRGB(final double h, final double saturation, final double lightness) {
        double hue = h;
        while (hue < 0)
            hue += 1;
        hue = hue - Math.floor(hue);
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
