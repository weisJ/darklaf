/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.properties.color;

import java.awt.*;

import javax.swing.*;

/** @author Jannis Weis */
public abstract class DarkColorModel {

    private final String prefix;
    private final String[] labels;

    public DarkColorModel(final String name, final String... labels) {
        this.prefix = "ColorChooser." + name;
        this.labels = labels;
    }

    public int getCount() {
        return this.labels.length;
    }

    public abstract int getMinimum(final int index);

    public abstract int getMaximum(final int index);

    public int getDefault(final int index) {
        return getMinimum(index);
    }

    public final String getText(final Component component, final String suffix) {
        return UIManager.getString(this.prefix + suffix + "Text", component.getLocale());
    }

    @Override
    public abstract String toString();

    public abstract String[] getLabelDescriptorsBefore();

    public String[] getFullLabelDescriptorsBefore() {
        return labels;
    }

    public String[] getLabelDescriptorsAfter() {
        return new String[] {"", "", "", ""};
    }

    public String[] getFullLabelDescriptorsAfter() {
        return getLabelDescriptorsAfter();
    }

    public abstract int[] getValuesFromColor(final Color color);

    public abstract Color getColorFromValues(final int[] values);
}
