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

public class DarkColorModelRGB extends DarkColorModel {

    private static final LazyValue<DarkColorModelRGB> instance = new LazyValue<>(DarkColorModelRGB::new);

    public static DarkColorModelRGB getInstance() {
        return instance.get();
    }

    public DarkColorModelRGB() {
        super("rgb", "Red", "Green", "Blue");
    }

    public int getMinimum(final int index) {
        return 0;
    }

    public int getMaximum(final int index) {
        return 255;
    }

    @Override
    public String toString() {
        return "RGB";
    }

    public String[] getLabelDescriptorsBefore() {
        return new String[]{"R", "G", "B"};
    }

    public String[] getLabelDescriptorsAfter() {
        return new String[]{"", "", "", ""};
    }

    public int[] getValuesFromColor(final Color color) {
        return new int[]{color.getRed(), color.getGreen(), color.getBlue()};
    }

    public int[] getValuesFromColorWithAlpha(final Color color) {
        return new int[]{color.getRed(), color.getGreen(), color.getBlue(), color.getAlpha()};
    }

    public Color getColorFromValues(final int[] values) {
        return new Color(values[0], values[1], values[2]);
    }
}
