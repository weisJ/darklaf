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
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.io.Serializable;
import java.net.URI;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.LogUtil;
import com.kitfox.svg.app.beans.SVGIcon;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since  2019
 */
public class DarkSVGIcon implements DerivableIcon<DarkSVGIcon>, RotateIcon, Serializable {

    private static final Logger LOGGER = LogUtil.getLogger(DarkSVGIcon.class);
    private final Dimension size;
    private final SVGIcon icon;
    private final URI uri;
    private final AtomicBoolean loaded;

    /**
     * Method to fetch the SVG icon from a url.
     *
     * @param uri           the uri from which to fetch the SVG icon.
     * @param displayWidth  display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(final URI uri, final int displayWidth, final int displayHeight) {
        this.uri = uri;
        size = new Dimension(displayWidth, displayHeight);
        icon = createSVGIcon();
        icon.setAutosize(SVGIcon.AUTOSIZE_STRETCH);
        icon.setAntiAlias(true);
        loaded = new AtomicBoolean(false);
    }

    private DarkSVGIcon(final int width, final int height, final DarkSVGIcon icon) {
        this.size = new Dimension(width, height);
        this.icon = icon.icon;
        this.uri = icon.uri;
        this.loaded = icon.loaded;
    }

    @Override
    public DarkSVGIcon derive(final int width, final int height) {
        return new DarkSVGIcon(width, height, this);
    }

    public void setDisplaySize(final int width, final int height) {
        size.setSize(width, height);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureLoaded();
        paintIcon(c, g, x, y, 0);
    }

    private void ensureLoaded() {
        if (!loaded.get()) {
            LOGGER.fine(() -> "Loading icon '" + uri.toASCIIString() + "'.");
            icon.setSvgURI(uri);
            loaded.set(true);
        }
    }

    protected SVGIcon createSVGIcon() {
        return new SVGIcon();
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y,
                          final double rotation) {
        ensureLoaded();
        Graphics2D g2 = (Graphics2D) g.create();
        g2.translate(x, y);
        if (rotation != 0) {
            g2.rotate(rotation, size.width / 2.0, size.height / 2.0);
        }
        icon.setPreferredSize(size);
        icon.paintIcon(c, g2, 0, 0);
        g2.dispose();
    }

    @Override
    public int getIconWidth() {
        return size.width;
    }

    @Override
    public int getIconHeight() {
        return size.height;
    }

    public SVGIcon getSVGIcon() {
        ensureLoaded();
        return icon;
    }
}
