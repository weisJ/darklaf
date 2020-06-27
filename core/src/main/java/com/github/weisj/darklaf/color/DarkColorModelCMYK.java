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

import com.github.weisj.darklaf.util.LazyValue;

/**
 * @author Jannis Weis
 */
public class DarkColorModelCMYK extends DarkColorModel {

    private static final int[] cmyk = new int[4];
    private static final int[] rgb = new int[3];

    private static final LazyValue<DarkColorModelCMYK> instance = new LazyValue<>(DarkColorModelCMYK::new);

    public static DarkColorModelCMYK getInstance() {
        return instance.get();
    }

    public DarkColorModelCMYK() {
        super("cmyk", "Cyan", "Magenta", "Yellow", "Black");
    }

    @Override
    public int getMinimum(final int index) {
        return 0;
    }

    @Override
    public int getMaximum(final int index) {
        return 100;
    }

    @Override
    public String toString() {
        return "CMYK";
    }

    @Override
    public String[] getLabelDescriptorsBefore() {
        return new String[]{"C", "M", "Y", "K"};
    }

    @Override
    public String[] getLabelDescriptorsAfter() {
        return new String[]{"%", "%", "%", "%"};
    }

    @Override
    public int[] getValuesFromColor(final Color color) {
        return RGBtoCMYK(color.getRed(), color.getGreen(), color.getBlue());
    }

    private static int[] RGBtoCMYK(final int r, final int g, final int b) {
        double max = DarkColorModelHSL.max(r / 255.0, g / 255.0, b / 255.0);
        if (max > 0.0f) {
            cmyk[0] = (int) Math.round((1.0f - (r / 255.0) / max) * 100);
            cmyk[1] = (int) Math.round((1.0f - (g / 255.0) / max) * 100);
            cmyk[2] = (int) Math.round((1.0f - (b / 255.0) / max) * 100);
        } else {
            cmyk[0] = 0;
            cmyk[1] = 0;
            cmyk[2] = 0;
        }
        cmyk[3] = (int) Math.round((1.0f - max) * 100);
        return cmyk;
    }

    @Override
    public Color getColorFromValues(final int[] values) {
        int[] rgb = CMYKtoRGB(values[0] / 100.0, values[1] / 100.0, values[2] / 100.0, values[3] / 100.0);
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    private static int[] CMYKtoRGB(final double c, final double m, final double y, final double k) {
        rgb[0] = (int) Math.round(255 * (1 - c) * (1 - k));
        rgb[1] = (int) Math.round(255 * (1 - m) * (1 - k));
        rgb[2] = (int) Math.round(255 * (1 - y) * (1 - k));
        return rgb;
    }
}
