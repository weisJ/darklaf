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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.awt.image.BufferedImage;

import com.github.weisj.darklaf.util.graphics.GraphicsUtil;
import com.github.weisj.darklaf.util.graphics.ScaledImage;


/**
 * Utility class for creating images from components.
 *
 * @author Jannis Weis
 */
public final class ImageUtil {

    private ImageUtil() {}

    /**
     * Create image from component.
     *
     * @param c the component.
     * @return image of the component.
     */
    public static ScaledImage imageFromComponent(final Component c) {
        return scaledImageFromComponent(c, new Rectangle(0, 0, c.getWidth(), c.getHeight()), 1.0, 1.0, true);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static ScaledImage imageFromComponent(final Component c, final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, true);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static ScaledImage imageFromComponent(final Component c, final Rectangle bounds, final boolean print) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, print);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @return image of the component.
     */
    public static ScaledImage scaledImageFromComponent(final Component c) {
        return scaledImageFromComponent(c, new Rectangle(0, 0, c.getWidth(), c.getHeight()), false);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static ScaledImage scaledImageFromComponent(final Component c, final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, false);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @param print whether the component should be painted in printing mode or not.
     * @return image containing the captured area.
     */
    public static ScaledImage scaledImageFromComponent(final Component c, final Rectangle bounds,
            final boolean print) {
        GraphicsConfiguration gc = c.getGraphicsConfiguration();
        return scaledImageFromComponent(c, bounds, Scale.getScaleX(gc), Scale.getScaleY(gc), print);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @param scalex the x scale
     * @param scaley the y scale
     * @return image containing the captured area.
     */
    public static ScaledImage scaledImageFromComponent(final Component c, final Rectangle bounds, final double scalex,
            final double scaley, final boolean print) {
        BufferedImage image;
        boolean scale = scalex != 1.0 || scaley != 1.0;
        if (scale) {
            image = createCompatibleTransparentImage(c.getGraphicsConfiguration(),
                    (int) (scalex * bounds.width), (int) (scaley * bounds.height));
        } else {
            image = createCompatibleTransparentImage(c.getGraphicsConfiguration(),
                    bounds.width, bounds.height);
        }
        final Graphics2D g2d = (Graphics2D) image.getGraphics();
        GraphicsUtil.setupAntialiasing(g2d);
        if (scale) {
            g2d.scale(scalex, scaley);
        }
        g2d.translate(-bounds.x, -bounds.y);
        if (print) {
            c.print(g2d);
        } else {
            c.paint(g2d);
        }

        g2d.dispose();
        return new ScaledImage(image, scalex, scaley);
    }

    public static BufferedImage createCompatibleImage(final GraphicsConfiguration gc,
            final int width, final int height) {
        return gc == null ? new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
                : gc.createCompatibleImage(width, height, Transparency.OPAQUE);
    }

    public static BufferedImage createCompatibleImage(final int width, final int height) {
        return createCompatibleImage(getGraphicsConfiguration(), width, height);
    }

    public static BufferedImage createCompatibleTransparentImage(final GraphicsConfiguration gc,
            final int width, final int height) {
        return gc == null ? new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                : gc.createCompatibleImage(width, height, Transparency.BITMASK);
    }

    public static BufferedImage createCompatibleTransparentImage(final int width, final int height) {
        return createCompatibleTransparentImage(getGraphicsConfiguration(), width, height);
    }

    public static BufferedImage createCompatibleTranslucentImage(final GraphicsConfiguration gc,
            final int width, final int height) {
        return gc == null ? new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                : gc.createCompatibleImage(width, height, Transparency.TRANSLUCENT);
    }

    public static BufferedImage createCompatibleTranslucentImage(final int width, final int height) {
        return createCompatibleTranslucentImage(getGraphicsConfiguration(), width, height);
    }

    private static boolean isHeadless() {
        return GraphicsEnvironment.isHeadless();
    }

    private static GraphicsConfiguration getGraphicsConfiguration() {
        return isHeadless()
                ? null
                : GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
    }
}
