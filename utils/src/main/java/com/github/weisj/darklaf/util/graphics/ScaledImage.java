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
package com.github.weisj.darklaf.util.graphics;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

public class ScaledImage extends Image {

    private final BufferedImage delegate;
    private final double scaleX;
    private final double scaleY;

    public ScaledImage(final BufferedImage delegate, final double scaleX, final double scaleY) {
        this.delegate = delegate;
        this.scaleX = scaleX;
        this.scaleY = scaleY;
    }

    public BufferedImage getDelegate() {
        return delegate;
    }

    public double getScaleX() {
        return scaleX;
    }

    public double getScaleY() {
        return scaleY;
    }

    @Override
    public int getWidth(final ImageObserver observer) {
        return delegate.getWidth(observer);
    }

    @Override
    public int getHeight(final ImageObserver observer) {
        return delegate.getHeight(observer);
    }

    @Override
    public ImageProducer getSource() {
        return delegate.getSource();
    }

    @Override
    public Graphics getGraphics() {
        return delegate.getGraphics();
    }

    @Override
    public Object getProperty(final String name, final ImageObserver observer) {
        return delegate.getProperty(name, observer);
    }
}
