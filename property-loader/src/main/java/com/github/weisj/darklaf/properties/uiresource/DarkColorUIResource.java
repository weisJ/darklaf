/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.properties.uiresource;

import java.awt.*;

import javax.swing.plaf.ColorUIResource;

public class DarkColorUIResource extends ColorUIResource {

    public DarkColorUIResource(final int r, final int g, final int b) {
        super(r, g, b);
    }

    public DarkColorUIResource(final int rgb) {
        super(rgb);
    }

    public DarkColorUIResource(final float r, final float g, final float b) {
        super(r, g, b);
    }

    public DarkColorUIResource(final Color c) {
        super(c);
    }

    @Override
    public Color darker() {
        return new DarkColorUIResource(super.darker());
    }

    @Override
    public Color brighter() {
        return new DarkColorUIResource(super.brighter());
    }

    @Override
    public String toString() {
        String s = "UIColor[r=" + format(getRed()) + ", g=" + format(getGreen()) + ",b=" + format(getBlue());
        if (getAlpha() != 255) {
            s += ",a=" + format(getAlpha());
        }
        s += "]";
        return s;
    }

    private String format(final int value) {
        return String.format("%03d", value);
    }
}
