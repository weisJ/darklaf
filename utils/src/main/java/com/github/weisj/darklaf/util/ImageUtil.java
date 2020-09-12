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
import java.awt.image.BufferedImage;

/** @author Jannis Weis */
public final class ImageUtil {

    private ImageUtil() {}

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static BufferedImage imageFromComponent(final Component c, final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, true);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static BufferedImage imageFromComponent(final Component c, final Rectangle bounds, final boolean print) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, print);
    }

    /**
     * Create image from component.
     *
     * @param c the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    public static BufferedImage scaledImageFromComponent(final Component c, final Rectangle bounds) {
        GraphicsConfiguration gc = c.getGraphicsConfiguration();
        return scaledImageFromComponent(c, bounds, Scale.getScaleX(gc), Scale.getScaleY(gc), true);
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
    public static BufferedImage scaledImageFromComponent(final Component c, final Rectangle bounds, final double scalex,
            final double scaley, final boolean print) {
        BufferedImage image;
        boolean scale = scalex != 1.0 || scaley != 1.0;
        if (scale) {
            image = createCompatibleTransparentImage((int) (scalex * bounds.width), (int) (scaley * bounds.height));
        } else {
            image = createCompatibleTransparentImage(bounds.width, bounds.height);
        }
        final Graphics2D g2d = (Graphics2D) image.getGraphics();
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
        return image;
    }

    public static BufferedImage createCompatibleImage(final int width, final int height) {
        return isHeadless() ? new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
                : getGraphicsConfiguration().createCompatibleImage(width, height, Transparency.OPAQUE);
    }

    public static BufferedImage createCompatibleTransparentImage(final int width, final int height) {
        return isHeadless() ? new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                : getGraphicsConfiguration().createCompatibleImage(width, height, Transparency.BITMASK);
    }

    public static BufferedImage createCompatibleTranslucentImage(final int width, final int height) {
        return isHeadless() ? new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                : getGraphicsConfiguration().createCompatibleImage(width, height, Transparency.TRANSLUCENT);
    }

    private static boolean isHeadless() {
        return GraphicsEnvironment.isHeadless();
    }

    private static GraphicsConfiguration getGraphicsConfiguration() {
        return GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
    }
}
