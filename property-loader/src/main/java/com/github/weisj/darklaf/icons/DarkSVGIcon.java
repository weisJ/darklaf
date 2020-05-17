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

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Scale;
import com.kitfox.svg.app.beans.SVGIcon;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since  2019
 */
public class DarkSVGIcon implements DerivableIcon<DarkSVGIcon>, RotateIcon, Serializable, ImageSource {

    private static final Logger LOGGER = LogUtil.getLogger(DarkSVGIcon.class);
    private final Dimension size;
    private final SVGIcon icon;
    protected final URI uri;
    private final AtomicBoolean loaded;
    private double scaleX;
    private double scaleY;
    private Image image;

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
        paintIcon(c, g, x, y, 0);
    }

    protected boolean ensureLoaded() {
        if (!loaded.get()) {
            LOGGER.fine(() -> "Loading icon '" + uri.toASCIIString() + "'.");
            icon.setSvgURI(uri);
            loaded.set(true);
            return true;
        }
        return false;
    }

    protected void updateCache(final boolean update, final Component c) {
        GraphicsConfiguration gc = c != null ? c.getGraphicsConfiguration() : null;
        double sx = Scale.getScaleX(gc);
        double sy = Scale.getScaleY(gc);
        if (!update && Scale.equalWithError(scaleX, sx) && Scale.equalWithError(scaleY, sy) && image != null) return;
        scaleX = sx;
        scaleY = sy;
        image = createImage(Scale.scale(scaleX, scaleY, size));
    }

    @Override
    public Image createImage(final Dimension size) {
        ensureLoaded();
        icon.setPreferredSize(size);
        return icon.getImage();
    }

    protected void ensureImageLoaded(final Component c) {
        updateCache(ensureLoaded(), c);
    }

    protected SVGIcon createSVGIcon() {
        return new SVGIcon();
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y,
                          final double rotation) {
        ensureImageLoaded(c);
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(x, y);
        double sx = size.width / (double) image.getWidth(null);
        double sy = size.height / (double) image.getWidth(null);
        g2.scale(sx, sy);
        if (rotation != 0) {
            g2.rotate(rotation, size.width / 2.0, size.height / 2.0);
        }
        g2.drawImage(image, 0, 0, null);
        g2.scale(1 / sx, 1 / sy);
        g2.translate(-x, -y);
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
        if (!loaded.get()) ensureLoaded();
        return icon;
    }
}
