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

import com.github.weisj.darklaf.util.LazyValue;

/**
 * @author Jannis Weis
 */
public class DarkColorModelHSB extends DarkColorModel {

    private static final float[] hsvVals = new float[3];
    private static final int[] hsb = new int[3];

    private static final LazyValue<DarkColorModelHSB> instance = new LazyValue<>(DarkColorModelHSB::new);

    public static DarkColorModelHSB getInstance() {
        return instance.get();
    }

    public DarkColorModelHSB() {
        super("hsv", "Hue", "Saturation", "Brightness");
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
        return "HSB";
    }

    @Override
    public String[] getLabelDescriptorsBefore() {
        return new String[] {"H", "S", "B"};
    }

    @Override
    public String[] getLabelDescriptorsAfter() {
        return new String[] {"\u00B0", "%", "%"};
    }

    @Override
    public int[] getValuesFromColor(final Color color) {
        return RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue());
    }

    private static int[] RGBtoHSB(final int r, final int g, final int b) {
        double[] values = RGBtoHSBValues(r, g, b);
        hsb[0] = (int) Math.round(values[0] * 360);
        hsb[1] = (int) Math.round(values[1] * 100);
        hsb[1] = (int) Math.round(values[2] * 100);
        return hsb;
    }

    public static double[] RGBtoHSBValues(final int r, final int g, final int b) {
        float[] values = Color.RGBtoHSB(r, g, b, hsvVals);
        return new double[] {values[0], values[1], values[2]};
    }

    public static Color getColorFromHSBValues(final double h, final double s, final double b) {
        return Color
            .getHSBColor((float) Math.max(Math.min(h, 1), 0), (float) Math.max(Math.min(s, 1), 0), (float) Math.max(Math.min(b, 1), 0));
    }

    @Override
    public Color getColorFromValues(final int[] values) {
        return getColorFromHSBValues(values[0] / 360.0, values[1] / 100.0, values[2] / 100.0);
    }
}
