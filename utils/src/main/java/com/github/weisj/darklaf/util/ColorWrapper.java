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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;

public class ColorWrapper extends Color {

    private Color color;

    public ColorWrapper(final Color color) {
        super(0);
        setColor(color);
    }

    public Color getColor() {
        return color;
    }

    public void setColor(final Color color) {
        if (color == null) {
            this.color = Color.BLACK;
        } else {
            this.color = color;
        }
    }

    @Override
    public int getRed() {
        return getColor().getRed();
    }

    @Override
    public int getGreen() {
        return getColor().getGreen();
    }

    @Override
    public int getBlue() {
        return getColor().getBlue();
    }

    @Override
    public int getAlpha() {
        return getColor().getAlpha();
    }

    @Override
    public int getRGB() {
        return getColor().getRGB();
    }

    @Override
    public Color brighter() {
        return getColor().brighter();
    }

    @Override
    public Color darker() {
        return getColor().darker();
    }

    @Override
    public float[] getRGBComponents(final float[] compArray) {
        return getColor().getRGBComponents(compArray);
    }

    @Override
    public float[] getRGBColorComponents(final float[] compArray) {
        return getColor().getRGBColorComponents(compArray);
    }

    @Override
    public float[] getComponents(final float[] compArray) {
        return getColor().getComponents(compArray);
    }

    @Override
    public float[] getColorComponents(final float[] compArray) {
        return getColor().getColorComponents(compArray);
    }

    @Override
    public float[] getComponents(final ColorSpace cspace, final float[] compArray) {
        return getColor().getComponents(cspace, compArray);
    }

    @Override
    public float[] getColorComponents(final ColorSpace cspace, final float[] compArray) {
        return getColor().getColorComponents(cspace, compArray);
    }

    @Override
    public ColorSpace getColorSpace() {
        return getColor().getColorSpace();
    }

    @Override
    public synchronized PaintContext createContext(final ColorModel cm, final Rectangle r, final Rectangle2D r2d,
            final AffineTransform xform, final RenderingHints hints) {
        return getColor().createContext(cm, r, r2d, xform, hints);
    }

    @Override
    public int getTransparency() {
        return getColor().getTransparency();
    }
}
