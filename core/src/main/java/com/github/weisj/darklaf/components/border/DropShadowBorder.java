/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.components.border;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.*;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import javax.swing.border.Border;

import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.Disposable;
import com.github.weisj.darklaf.util.ImageUtil;

/**
 * Implements a DropShadow for components. In general, the DropShadowBorder will work with any
 * rectangular components that do not have a default border installed as part of the look and feel,
 * or otherwise. For example, DropShadowBorder works wonderfully with JPanel, but horribly with
 * JComboBox.
 *
 * <p>
 * Note: {@code DropShadowBorder} should usually be added to non-opaque components, otherwise the
 * background is likely to bleed through.
 *
 * <p>
 * Note: Since generating drop shadows is relatively expensive operation, {@code
 * DropShadowBorder} keeps internal static cache that allows sharing same border for multiple
 * re-rendering and between different instances of the class. Since this cache is shared at class
 * level and never reset, it might bleed your app memory in case you tend to create many borders
 * rapidly.
 *
 * @author rbair Adaptions made by
 * @author Jannis Weis
 */
public class DropShadowBorder implements Border, Serializable {
    private static final Map<ImageHashKey, BufferedImage[]> CACHE = new HashMap<>();
    private Color shadowColor;
    private int shadowSize;
    private float shadowOpacity;
    private int cornerSize;

    public DropShadowBorder() {
        this(Color.BLACK, 5);
    }

    public DropShadowBorder(final Color shadowColor, final int shadowSize) {
        this(shadowColor, shadowSize, .5f, 12);
    }

    public DropShadowBorder(final Color shadowColor, final int shadowSize, final float shadowOpacity,
            final int cornerSize) {
        setShadowColor(shadowColor);
        setShadowSize(shadowSize);
        setShadowOpacity(shadowOpacity);
        setCornerSize(cornerSize);
    }

    @Override
    public void paintBorder(final Component c, final Graphics graphics, final int x, final int y, final int width,
            final int height) {
        final BufferedImage[] images = getImages();
        final Graphics2D g2 = (Graphics2D) graphics.create();

        try (Disposable ignored = g2::dispose) {
            // The location and size of the shadows depends on which shadows are being
            // drawn. For instance, if the left & bottom shadows are being drawn, then
            // the left shadow extends all the way down to the corner, a corner is drawn,
            // and then the bottom shadow begins at the corner. If, however, only the
            // bottom shadow is drawn, then the bottom-left corner is drawn to the
            // right of the corner, and the bottom shadow is somewhat shorter than before.

            int cornerImageSize = cornerImageSize();

            Point topLeftShadowPoint = new Point(x, y);
            Point bottomLeftShadowPoint = new Point(x, y + height - cornerImageSize);
            Point bottomRightShadowPoint = new Point(x + width - cornerImageSize, y + height - cornerImageSize);
            Point topRightShadowPoint = new Point(x + width - cornerImageSize, y);

            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED);

            drawImage(g2, images[Position.LEFT.ordinal()],
                    topLeftShadowPoint.x, topLeftShadowPoint.y + cornerImageSize,
                    shadowSize, bottomLeftShadowPoint.y - topLeftShadowPoint.y - cornerImageSize, c);
            drawImage(g2, images[Position.BOTTOM.ordinal()],
                    bottomLeftShadowPoint.x + cornerImageSize, y + height - shadowSize,
                    bottomRightShadowPoint.x - bottomLeftShadowPoint.x - cornerImageSize, shadowSize, c);
            drawImage(g2, images[Position.RIGHT.ordinal()],
                    x + width - shadowSize, topRightShadowPoint.y + cornerImageSize, shadowSize,
                    bottomRightShadowPoint.y - topRightShadowPoint.y - cornerImageSize, c);
            drawImage(g2, images[Position.TOP.ordinal()],
                    topLeftShadowPoint.x + cornerImageSize, topLeftShadowPoint.y,
                    topRightShadowPoint.x - topLeftShadowPoint.x - cornerImageSize, shadowSize, c);

            drawImage(g2, images[Position.TOP_LEFT.ordinal()],
                    topLeftShadowPoint.x, topLeftShadowPoint.y,
                    cornerImageSize, cornerImageSize, c);
            drawImage(g2, images[Position.BOTTOM_LEFT.ordinal()],
                    bottomLeftShadowPoint.x, bottomLeftShadowPoint.y,
                    cornerImageSize, cornerImageSize, c);
            drawImage(g2, images[Position.BOTTOM_RIGHT.ordinal()],
                    bottomRightShadowPoint.x, bottomRightShadowPoint.y,
                    cornerImageSize, cornerImageSize, c);
            drawImage(g2, images[Position.TOP_RIGHT.ordinal()],
                    topRightShadowPoint.x, topRightShadowPoint.y,
                    cornerImageSize, cornerImageSize, c);
        }
    }

    protected void drawImage(final Graphics g, final Image image, final int x, final int y, final int w, final int h,
            final ImageObserver observer) {
        g.drawImage(image, x, y, w, h, observer);
    }

    private int cornerImageSize() {
        return getCornerSize() + getShadowSize();
    }

    private BufferedImage[] getImages() {
        // first, check to see if an image for this size has already been rendered
        // if so, use the cache. Else, draw and save
        BufferedImage[] images = CACHE.get(new ImageHashKey(shadowSize, cornerSize, shadowColor, shadowOpacity));
        if (images == null) {
            images = new BufferedImage[Position.count()];

            /*
             * To draw a drop shadow, I have to:
             *
             * 1) Create a rounded rectangle
             *
             * 2) Create a BufferedImage to draw the rounded rect in
             *
             * 3) Translate the graphics for the image, so that the rectangle is centered in the drawn space.
             * The border around the rectangle needs to be shadowWidth wide, so that there is space for the
             * shadow to be drawn.
             *
             * 4) Draw the rounded rect as shadowColor, with an opacity of shadowOpacity
             *
             * 5) Create the BLUR_KERNEL
             *
             * 6) Blur the image
             *
             * 7) copy off the corners, sides, etc. into images to be used for drawing the Border
             */
            int rectArc = shadowSize + cornerSize;
            int rectWidth = 2 * rectArc + 1;
            RoundRectangle2D rect = new RoundRectangle2D.Double(
                    0, 0, rectWidth, rectWidth, rectArc, rectArc);
            final int kernelSize = (3 * shadowSize) / 4;
            final int imageSize = rectWidth + 2 * kernelSize;
            BufferedImage image = ImageUtil.createCompatibleTranslucentImage(imageSize, imageSize);
            Graphics2D buffer = (Graphics2D) image.getGraphics();

            try (Disposable ignored = buffer::dispose) {
                buffer.setPaint(ColorUtil.toAlpha(shadowColor, shadowOpacity));
                double offset = (imageSize - rectWidth) / 2.0;
                buffer.translate(offset, offset);
                buffer.fill(rect);
            }

            float blurry = 1.0f / (float) (kernelSize * kernelSize);
            float[] blurKernel = new float[kernelSize * kernelSize];
            Arrays.fill(blurKernel, blurry);
            ConvolveOp blur = new ConvolveOp(new Kernel(kernelSize, kernelSize, blurKernel));

            BufferedImage targetImage = blur.filter(image, null);

            int cornerImageSize = cornerImageSize();
            int sideImageSize = shadowSize;

            int cornerOppositeOffset = imageSize - cornerImageSize;
            int sideOppositeOffset = imageSize - sideImageSize;

            images[Position.TOP_LEFT.ordinal()] = getSubImage(targetImage,
                    0, 0, cornerImageSize, cornerImageSize);
            images[Position.BOTTOM_LEFT.ordinal()] = getSubImage(targetImage,
                    0, cornerOppositeOffset, cornerImageSize, cornerImageSize);
            images[Position.BOTTOM_RIGHT.ordinal()] = getSubImage(targetImage,
                    cornerOppositeOffset, cornerOppositeOffset, cornerImageSize, cornerImageSize);
            images[Position.TOP_RIGHT.ordinal()] = getSubImage(targetImage,
                    cornerOppositeOffset, 0, cornerImageSize, cornerImageSize);

            images[Position.LEFT.ordinal()] = getSubImage(targetImage,
                    0, cornerImageSize, sideImageSize, 1);
            images[Position.BOTTOM.ordinal()] = getSubImage(targetImage,
                    cornerImageSize, sideOppositeOffset, 1, sideImageSize);
            images[Position.RIGHT.ordinal()] = getSubImage(targetImage,
                    sideOppositeOffset, cornerImageSize, sideImageSize, 1);
            images[Position.TOP.ordinal()] = getSubImage(targetImage,
                    cornerImageSize, 0, 1, sideImageSize);

            image.flush();
            targetImage.flush();
            CACHE.put(new ImageHashKey(shadowSize, cornerSize, shadowColor, shadowOpacity), images);
        }
        return images;
    }

    /**
     * Returns a new BufferedImage that represents a subregion of the given BufferedImage. (Note that
     * this method does not use BufferedImage.getSubimage(), which will defeat image acceleration
     * strategies on later JDKs.)
     */
    private BufferedImage getSubImage(final BufferedImage img, final int x, final int y, final int w, final int h) {
        BufferedImage ret = ImageUtil.createCompatibleTranslucentImage(w, h);
        Graphics2D g = ret.createGraphics();
        try (Disposable ignored = g::dispose) {
            g.drawImage(img, 0, 0, w, h, x, y, x + w, y + h, null);
        }
        return ret;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Insets getBorderInsets(final Component c) {
        return new Insets(shadowSize, shadowSize, shadowSize, shadowSize);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    public int getShadowSize() {
        return shadowSize;
    }

    public void setShadowSize(final int shadowSize) {
        this.shadowSize = shadowSize;
    }

    public Color getShadowColor() {
        return shadowColor;
    }

    public void setShadowColor(final Color shadowColor) {
        this.shadowColor = shadowColor;
        if (shadowColor == null) this.shadowColor = Color.BLACK;
    }

    public float getShadowOpacity() {
        return shadowOpacity;
    }

    public void setShadowOpacity(final float shadowOpacity) {
        this.shadowOpacity = shadowOpacity;
    }

    public int getCornerSize() {
        return cornerSize;
    }

    public void setCornerSize(final int cornerSize) {
        this.cornerSize = Math.min(shadowSize, cornerSize);
    }

    private enum Position {
        TOP,
        TOP_LEFT,
        LEFT,
        BOTTOM_LEFT,
        BOTTOM,
        BOTTOM_RIGHT,
        RIGHT,
        TOP_RIGHT;

        static int count() {
            return 8;
        }
    }

    private static final class ImageHashKey {
        private final int shadowSize;
        private final int cornerSize;
        private final Color shadowColor;
        private final float opacity;

        private ImageHashKey(final int shadowSize, final int cornerSize, final Color shadowColor, final float opacity) {
            this.shadowSize = shadowSize;
            this.cornerSize = cornerSize;
            this.shadowColor = shadowColor;
            this.opacity = opacity;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof ImageHashKey)) return false;
            ImageHashKey that = (ImageHashKey) o;
            return shadowSize == that.shadowSize
                    && cornerSize == that.cornerSize
                    && Float.compare(that.opacity, opacity) == 0
                    && Objects.equals(shadowColor, that.shadowColor);
        }

        @Override
        public int hashCode() {
            return Objects.hash(shadowSize, cornerSize, shadowColor, opacity);
        }
    }
}
