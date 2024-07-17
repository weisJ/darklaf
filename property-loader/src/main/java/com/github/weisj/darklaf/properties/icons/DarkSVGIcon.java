/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.BaseMultiResolutionImage;
import java.awt.image.BufferedImage;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Scale;
import com.github.weisj.jsvg.SVGDocument;
import com.github.weisj.jsvg.attributes.ViewBox;
import com.github.weisj.jsvg.geometry.size.FloatSize;
import com.github.weisj.jsvg.parser.DefaultParserProvider;
import com.github.weisj.jsvg.parser.ParserProvider;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkSVGIcon extends ImageIcon
        implements DerivableIcon<DarkSVGIcon>, IconLoader.CacheableIcon, RotateIcon, Serializable, ImageSource,
        VisualPaddingProvider {

    private static final Logger LOGGER = LogUtil.getLogger(DarkSVGIcon.class);

    /*
     * Render the icon a bit larger than needed to ensure it is painted good enough when rotated. This
     * accounts for diagonal lines (i.e. when the rotation is pi/4). Ideally this value would only need
     * to be sqrt(2) but 2.0 behaves a lot better w.r.t. floating point calculations.
     *
     * The scale factor is only used if the icon is painted with a non-trivial rotation.
     */
    private static final double extraScale = 2.0;

    private final @NotNull Dimension iconSize;

    private final @NotNull SVGDocumentHolder svgDocumentHolder;

    private IconLoader.IconKey iconKey;

    private boolean directRendering;
    private boolean loadedWithExtraScale;
    private double scaleX;
    private double scaleY;
    private Image image;
    private BaseMultiResolutionImage multiResolutionImage;

    /**
     * Method to fetch the SVG icon from an url.
     *
     * @param uri the svg uri.
     * @param displayWidth display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(final @NotNull URI uri, final int displayWidth, final int displayHeight) {
        this.svgDocumentHolder = new SVGDocumentHolder(uri);
        iconSize = new Dimension(displayWidth, displayHeight);
    }

    protected DarkSVGIcon(final int width, final int height, final DarkSVGIcon parent) {
        this.iconSize = new Dimension(width, height);
        this.svgDocumentHolder = parent.svgDocumentHolder;
    }

    @Override
    public void setCacheKey(final IconLoader.IconKey key) {
        this.iconKey = key;
    }

    IconLoader.IconKey getCacheKey() {
        return iconKey;
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
        return svgDocumentHolder.ensureLoaded(this);
    }

    protected @NotNull ParserProvider createParserProvider() {
        return new DefaultParserProvider();
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
                getSize().width, getSize().height, effectiveScaleX, effectiveScaleX, getName(getURI())));
        image = createImage(Scale.scale(effectiveScaleX, effectiveScaleY, getSize()));

        // Invalidate the multi resolution image
        multiResolutionImage = null;
    }

    @Override
    public Image createImage(final Dimension size) {
        ensureLoaded(false);
        BufferedImage bi = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = (Graphics2D) bi.getGraphics();
        try {
            g.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g.setRenderingHint(
                    RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
            Object aaHint = UIManager.get(RenderingHints.KEY_TEXT_ANTIALIASING);
            if (aaHint != null) g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, aaHint);
            svgDocumentHolder.svgDocument.render(null, g, new ViewBox(0, 0, size.width, size.height));
        } catch (final RuntimeException e) {
            LOGGER.log(Level.SEVERE, "Exception while painting '" + getURI().toASCIIString() + "'.", e);
        } finally {
            g.dispose();
        }
        return bi;
    }

    @Override
    public @NotNull Image getImage() {
        if (multiResolutionImage == null) {
            multiResolutionImage = new BaseMultiResolutionImage(
                    createImage(getSize()),
                    createImage(Scale.scale(2, 2, getSize())));
        }
        return multiResolutionImage;
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
        return Scale.equalWithError(r, 0)
                || Scale.equalWithError(r, Math.PI / 2)
                || Scale.equalWithError(r, Math.PI)
                || Scale.equalWithError(r, 3 * Math.PI / 2);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y, final double rotation) {
        boolean dr = isDirectRenderingMode();
        if (dr) {
            ensureLoaded(true);
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
        if (!dr) g2.scale(sx, sy);
        if (rotation != 0) {
            g2.rotate(rotation, imageWidth / 2.0, imageHeight / 2.0);
        }

        if (dr) {
            SVGDocument svg = getSVGDocument();
            svg.render((JComponent) c, (Graphics2D) g, new ViewBox(0, 0, size.width, size.height));
        } else {
            g2.drawImage(image, 0, 0, c);
            g2.scale(1 / sx, 1 / sy);
        }

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
            SVGDocument svg = getSVGDocument();
            FloatSize svgSize = svg.size();
            int width = (int) (svgSize.width + 0.5);
            int height = (int) (svgSize.height + 0.5);

            if (iconSize.height < 0 && iconSize.width >= 0) {
                height = (int) ((iconSize.width * height) / (double) width);
                width = iconSize.width;
            } else if (iconSize.height >= 0 && iconSize.width < 0) {
                width = (int) ((iconSize.height * width) / (double) height);
                height = iconSize.height;
            } else if (iconSize.width == iconSize.height && iconSize.height < -1) {
                // Scale to make the largest side fit the given size.
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
            LOGGER.finer(() -> "Inferred size of icon '" + getName(getURI()) + "' to " + iconSize);
        }
    }

    public SVGDocument getSVGDocument() {
        return svgDocumentHolder.svgDocument(this);
    }

    @Override
    public @NotNull Insets getVisualPaddings(@NotNull Component component) {
        return svgDocumentHolder.visualPaddings(this);
    }

    void setVisualPadding(final Insets visualPadding) {
        this.svgDocumentHolder.visualPadding = visualPadding;
    }

    public URI getURI() {
        return svgDocumentHolder.uri;
    }

    @Override
    public String toString() {
        return "DarkSVGIcon{" +
                ", svgDocumentHolder=" + svgDocumentHolder +
                ", iconSize=" + iconSize +
                ", iconKey=" + iconKey +
                ", directRendering=" + directRendering +
                ", loadedWithExtraScale=" + loadedWithExtraScale +
                ", scaleX=" + scaleX +
                ", scaleY=" + scaleY +
                ", image=" + image +
                '}';
    }

    private static class SVGDocumentHolder {
        private final @NotNull AtomicBoolean loaded = new AtomicBoolean();
        private SVGDocument svgDocument;
        private final @NotNull URI uri;
        private Insets visualPadding;

        private SVGDocumentHolder(final @NotNull URI uri) {
            this(uri, null);
        }

        private SVGDocumentHolder(final @NotNull URI uri, final SVGDocument svgDocument) {
            this.uri = uri;
            this.svgDocument = svgDocument;
        }

        private boolean ensureLoaded(final @NotNull DarkSVGIcon darkSVGIcon) {
            if (!loaded.get()) {
                URI iconUri = uri;
                LOGGER.finer(() -> "Loading icon '" + iconUri.toASCIIString() + "'.");
                try {
                    svgDocument = IconLoader.svgLoader().load(uri.toURL(), darkSVGIcon.createParserProvider());
                } catch (MalformedURLException e) {
                    LOGGER.log(Level.SEVERE, e.getMessage(), e);
                }
                Objects.requireNonNull(svgDocument, () -> "Document failed to load: " + iconUri.toASCIIString());
                loaded.set(true);
                return true;
            }
            return false;
        }

        private @NotNull SVGDocument svgDocument(final @NotNull DarkSVGIcon darkSVGIcon) {
            ensureLoaded(darkSVGIcon);
            return svgDocument;
        }

        private @NotNull Insets visualPaddings(final @NotNull DarkSVGIcon darkSVGIcon) {
            ensureLoaded(darkSVGIcon);
            return visualPadding != null ? visualPadding : new Insets(0, 0, 0, 0);
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof SVGDocumentHolder that)) return false;
            return (loaded.get() == that.loaded.get())
                    && Objects.equals(svgDocument, that.svgDocument)
                    && uri.equals(that.uri)
                    && Objects.equals(visualPadding, that.visualPadding);
        }

        @Override
        public int hashCode() {
            return Objects.hash(loaded, svgDocument, uri, visualPadding);
        }

        @Override
        public String toString() {
            return "SVGDocumentHolder{" +
                    "loaded=" + loaded +
                    ", svgDocument=" + Objects.hash(svgDocument) +
                    ", uri=" + uri +
                    ", visualPadding=" + visualPadding +
                    '}';
        }
    }
}
