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
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.Serializable;
import java.net.URI;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;
import java.util.logging.Logger;

import javax.swing.UIManager;

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Scale;
import com.kitfox.svg.app.beans.SVGIcon;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkSVGIcon implements DerivableIcon<DarkSVGIcon>, RotateIcon, Serializable, ImageSource {

    private static final Logger LOGGER = LogUtil.getLogger(DarkSVGIcon.class);

    /*
     * Render the icon a bit larger than needed to ensure it is painted good enough when rotated. This
     * accounts for diagonal lines (i.e. when the rotation is pi/4). Ideally this value would only need
     * to be sqrt(2) but 2.0 behaves a lot better w.r.t. floating point calculations.
     *
     * The scale factor is only used if the icon is painted with a non trivial rotation.
     */
    private static final double extraScale = 2.0;

    private final AtomicBoolean loaded;
    private final Dimension iconSize;
    private final SVGIcon icon;

    private Supplier<URI> uriSupplier;
    private URI uri;

    private IconLoader.IconKey iconKey;

    private boolean directRendering;
    private boolean loadedWithExtraScale;
    private double scaleX;
    private double scaleY;
    private Image image;

    /**
     * Method to fetch the SVG icon from a url.
     *
     * @param uriSupplier supplier for the uri from which to fetch the SVG icon.
     * @param displayWidth display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(final Supplier<URI> uriSupplier, final int displayWidth, final int displayHeight) {
        this.uri = null;
        this.uriSupplier = uriSupplier;
        iconSize = new Dimension(displayWidth, displayHeight);
        icon = createSVGIcon();
        icon.setAutosize(SVGIcon.AUTOSIZE_STRETCH);
        icon.setAntiAlias(true);
        loaded = new AtomicBoolean(false);
    }

    /**
     * Method to fetch the SVG icon from a url.
     *
     * @param uri the uri from which to fetch the SVG icon.
     * @param displayWidth display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(final URI uri, final int displayWidth, final int displayHeight) {
        this.uri = uri;
        uriSupplier = null;
        iconSize = new Dimension(displayWidth, displayHeight);
        icon = createSVGIcon();
        icon.setAutosize(SVGIcon.AUTOSIZE_STRETCH);
        icon.setAntiAlias(true);
        loaded = new AtomicBoolean(false);
    }

    protected DarkSVGIcon(final int width, final int height, final DarkSVGIcon parent) {
        this.iconSize = new Dimension(width, height);
        this.icon = parent.icon;
        this.uri = parent.uri;
        this.uriSupplier = parent.uriSupplier;
        this.loaded = parent.loaded;
    }

    void setIconKey(final IconLoader.IconKey iconKey) {
        this.iconKey = iconKey;
    }

    @Override
    public DarkSVGIcon derive(final int width, final int height) {
        if (width == getIconWidth() && height == getIconHeight()) {
            // Even though checking the size may cause the icon to be loaded, we
            // do this optimization as painting to different off-screen images is more
            // expensive than loading the icon.
            return this;
        }
        return new DarkSVGIcon(width, height, this);
    }

    public void setDisplaySize(final int width, final int height) {
        if (iconKey != null) {
            iconKey.w = width;
            iconKey.h = height;
        }
        iconSize.setSize(width, height);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        paintIcon(c, g, x, y, 0);
    }

    protected boolean ensureLoaded(final boolean painting) {
        return ensureSVGLoaded();
    }

    protected URI getUri() {
        ensureURILoaded();
        return uri;
    }

    private boolean ensureSVGLoaded() {
        if (!isSVGLoaded()) {
            URI iconUri = getUri();
            LOGGER.finer(() -> "Loading icon '" + iconUri.toASCIIString() + "'.");
            icon.setSvgURI(iconUri);
            loaded.set(true);
            return true;
        }
        return false;
    }

    private boolean isSVGLoaded() {
        return loaded.get();
    }

    private void ensureURILoaded() {
        if (uri == null && uriSupplier != null) {
            uri = uriSupplier.get();
            uriSupplier = null;
        }
        if (uri == null) {
            throw new IllegalStateException("Uri is null.");
        }
    }

    protected void updateCache(final boolean update, final Component c) {
        GraphicsConfiguration gc = c != null ? c.getGraphicsConfiguration() : null;
        double sx = Scale.getScaleX(gc);
        double sy = Scale.getScaleY(gc);
        if (!update && Scale.equalWithError(scaleX, sx) && Scale.equalWithError(scaleY, sy) && image != null) return;
        scaleX = sx;
        scaleY = sy;
        double effectiveScaleX = loadedWithExtraScale ? scaleX * extraScale : scaleX;
        double effectiveScaleY = loadedWithExtraScale ? scaleY * extraScale : scaleY;
        LOGGER.finer(() -> String.format("Creating Image with size (w=%s, h=%s, scaleW=%s, scaleH=%s) for icon '%s'",
                getSize().width, getSize().height, effectiveScaleX, effectiveScaleX, getName(getUri())));
        image = createImage(Scale.scale(effectiveScaleX, effectiveScaleY, getSize()));
    }

    @Override
    public Image createImage(final Dimension size) {
        ensureLoaded(false);
        icon.setPreferredSize(size);
        try {
            BufferedImage bi = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = (Graphics2D) bi.getGraphics();
            g.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g.setRenderingHint(
                    RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
            g.setRenderingHint(
                    RenderingHints.KEY_TEXT_ANTIALIASING, UIManager.get(RenderingHints.KEY_TEXT_ANTIALIASING));
            icon.paintIcon(null, g, 0, 0);
            g.dispose();
            return bi;
        } catch (final RuntimeException e) {
            if (!(this instanceof ThemedSVGIcon)) {
                IconColorMapper.patchColors(icon);
                Image img = icon.getImage();
                /*
                 * If we get to here the issue was that the icon hasn't been patched because it isn't loaded as a
                 * themed svg icon.
                 */
                LOGGER.severe("Icon '" + getName(uri) + "' that defines custom colors isn't loaded as themed icon.");
                return img;
            }
            throw new RuntimeException("Exception while painting '" + uri.toASCIIString() + "'.", e);
        }
    }

    protected String getName(final URI uri) {
        String name = uri.toASCIIString();
        name = name.substring(Math.min(name.length() - 1, name.lastIndexOf('/') + 1));
        return name;
    }

    protected void ensureImageLoaded(final Component c, final double rotation) {
        loadedWithExtraScale = !isExactRotation(rotation);
        updateCache(ensureLoaded(true) || loadedWithExtraScale, c);
    }

    private boolean isExactRotation(final double rotation) {
        double r = rotation;
        if (r < 0) r += 2 * Math.PI;
        if (r > 2 * Math.PI) r -= 2 * Math.PI;
        return Scale.equalWithError(r, 0) || Scale.equalWithError(r, Math.PI / 2) || Scale.equalWithError(r, Math.PI)
                || Scale.equalWithError(r, 3 * Math.PI / 2);
    }

    protected SVGIcon createSVGIcon() {
        return new SVGIcon();
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y, final double rotation) {
        boolean dr = isDirectRenderingMode();
        if (dr) {
            ensureLoaded(true);
            icon.setPreferredSize(getSize());
        } else {
            ensureImageLoaded(c, rotation);
        }

        Graphics2D g2 = (Graphics2D) g;
        AffineTransform transform = g2.getTransform();
        g2.translate(x, y);

        Dimension size = getSize();
        double imageWidth = dr ? size.width : image.getWidth(null);
        double imageHeight = dr ? size.height : image.getHeight(null);
        double sx = size.width / imageWidth;
        double sy = size.height / imageHeight;
        g2.scale(sx, sy);
        if (rotation != 0) {
            g2.rotate(rotation, imageWidth / 2.0, imageHeight / 2.0);
        }

        if (dr) {
            getSVGIcon().paintIcon(c, g, 0, 0);
        } else {
            g2.drawImage(image, 0, 0, null);
        }
        g2.scale(1 / sx, 1 / sy);
        g2.translate(-x, -y);
        g2.setTransform(transform);
    }

    public boolean isDirectRenderingMode() {
        return directRendering;
    }

    public void setDirectRenderingMode(final boolean directRendering) {
        this.directRendering = directRendering;
    }

    @Override
    public int getIconWidth() {
        ensureSizeLoaded();
        return getSize().width;
    }

    @Override
    public int getIconHeight() {
        return getSize().height;
    }

    protected Dimension getSize() {
        ensureSizeLoaded();
        return iconSize;
    }

    private void ensureSizeLoaded() {
        if (iconSize.width < 0 || iconSize.height < 0) {
            SVGIcon svg = getSVGIcon();
            int autoSizeMode = svg.getAutosize();
            svg.setAutosize(SVGIcon.AUTOSIZE_NONE);
            int width = svg.getIconWidthIgnoreAutosize();
            int height = svg.getIconHeightIgnoreAutosize();
            svg.setAutosize(autoSizeMode);

            if (iconSize.height < 0 && iconSize.width >= 0) {
                height = (int) ((iconSize.width * height) / (double) width);
                width = iconSize.width;
            } else if (iconSize.height >= 0 && iconSize.width < 0) {
                width = (int) ((iconSize.height * width) / (double) height);
                height = iconSize.height;
            } else if (iconSize.width == iconSize.height && iconSize.height < -1) {
                // Scale to make largest side fit the given size.
                int size = Math.abs(iconSize.width);
                if (width == height) {
                    width = size;
                    height = size;
                } else if (width > height) {
                    height = (int) ((size * height) / (double) width);
                    width = size;
                } else {
                    width = (int) ((size * width) / (double) height);
                    height = size;
                }
            }
            setDisplaySize(width, height);
            LOGGER.finer(() -> "Inferred size of icon '" + getName(uri) + "' to " + iconSize);
        }
    }

    public SVGIcon getSVGIcon() {
        ensureSVGLoaded();
        return icon;
    }

    @Override
    public String toString() {
        return "DarkSVGIcon{" +
                "loaded=" + loaded +
                ", iconSize=" + iconSize +
                ", icon=" + icon +
                ", uriSupplier=" + uriSupplier +
                ", uri=" + uri +
                ", iconKey=" + iconKey +
                ", directRendering=" + directRendering +
                ", loadedWithExtraScale=" + loadedWithExtraScale +
                ", scaleX=" + scaleX +
                ", scaleY=" + scaleY +
                ", image=" + image +
                '}';
    }
}
