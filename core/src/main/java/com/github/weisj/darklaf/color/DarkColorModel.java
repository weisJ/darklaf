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
package com.github.weisj.darklaf.color;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkColorModel {

    private final String prefix;
    private final String[] labels;


    public DarkColorModel() {
        this("rgb", "Red", "Green", "Blue");
    }


    public DarkColorModel(final String name, final String... labels) {
        this.prefix = "ColorChooser." + name;
        this.labels = labels;
    }

    public int getCount() {
        return this.labels.length;
    }

    public int getMinimum(final int index) {
        return 0;
    }

    public int getMaximum(final int index) {
        return 255;
    }

    public int getDefault(final int index) {
        return 0;
    }

    public final String getText(final Component component, final String suffix) {
        return UIManager.getString(this.prefix + suffix + "Text", component.getLocale());
    }

    @Override
    public String toString() {
        return "RGB";
    }

    public int getValueCount() {
        return 3;
    }

    public char[] getLabelDescriptorsBefore() {
        return new char[]{'R', 'G', 'B'};
    }

    public char[] getLabelDescriptorsAfter() {
        return new char[]{Character.MIN_VALUE, Character.MIN_VALUE, Character.MIN_VALUE, Character.MIN_VALUE};
    }

    public int[] getValuesFromColor(final Color color) {
        return new int[]{color.getRed(), color.getGreen(), color.getBlue()};
    }

    public Color getColorFromValues(final int[] values) {
        return new Color(values[0], values[1], values[2]);
    }
}
