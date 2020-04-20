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
 */
package com.github.weisj.darklaf.graphics;

import com.github.weisj.darklaf.util.Scale;

import java.awt.*;
import java.awt.image.BufferedImage;

import javax.swing.*;

/**
 * @author Jannis Weis
 */
public final class ImageUtil {

    private ImageUtil() {}

    private static final int FRAME_ICON_SIZE = 32;

    public static Image createFrameIcon(final Icon icon) {
        int w = icon.getIconWidth();
        int h = icon.getIconHeight();
        double scaleX = Scale.SCALE_X * (((double) FRAME_ICON_SIZE) / w);
        double scaleY = Scale.SCALE_Y * (((double) FRAME_ICON_SIZE) / h);
        return createScaledImage(icon, scaleX, scaleY);
    }

    public static Image createScaledImage(final Icon icon, final double scalex, final double scaley) {
        int w = (int) (scalex * icon.getIconWidth());
        int h = (int) (scaley * icon.getIconHeight());
        BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = (Graphics2D) image.getGraphics();
        g.scale(scalex, scaley);
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    public static Image createDragImage(final Component c, final int lw, final Color borderColor) {
        return createDragImage(c, new Rectangle(0, 0, c.getWidth(), c.getHeight()), lw, borderColor);
    }

    public static Image createDragImage(final Component c, final Rectangle bounds,
                                        final int lw, final Color borderColor) {
        Image tabImage = ImageUtil.scaledImageFromComponent(c, bounds);
        int w = tabImage.getWidth(null);
        int h = tabImage.getHeight(null);
        Graphics g = tabImage.getGraphics();

        g.setColor(borderColor);
        PaintUtil.drawRect(g, 0, 0, w, h, lw);
        g.dispose();
        return tabImage;
    }

    /**
     * Create image from component.
     *
     * @param  c      the component.
     * @param  bounds the bounds inside the component to capture.
     * @return        image containing the captured area.
     */
    public static BufferedImage imageFromComponent(final Component c, final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, true);
    }

    /**
     * Create image from component.
     *
     * @param  c      the component.
     * @param  bounds the bounds inside the component to capture.
     * @return        image containing the captured area.
     */
    public static BufferedImage imageFromComponent(final Component c, final Rectangle bounds, final boolean print) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0, print);
    }

    /**
     * Create image from component.
     *
     * @param  c      the component.
     * @param  bounds the bounds inside the component to capture.
     * @return        image containing the captured area.
     */
    public static BufferedImage scaledImageFromComponent(final Component c, final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, Scale.SCALE_X, Scale.SCALE_Y, true);
    }

    /**
     * Create image from component.
     *
     * @param  c      the component.
     * @param  bounds the bounds inside the component to capture.
     * @param  scalex the x scale
     * @param  scaley the y scale
     * @return        image containing the captured area.
     */
    public static BufferedImage scaledImageFromComponent(final Component c, final Rectangle bounds,
                                                         final double scalex, final double scaley,
                                                         final boolean print) {
        BufferedImage image;
        boolean scale = scalex != 1.0 || scaley != 1.0;
        if (scale) {
            image = new BufferedImage((int) (scalex * bounds.width), (int) (scaley * bounds.height),
                                      BufferedImage.TYPE_INT_RGB);
        } else {
            image = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_INT_RGB);
        }
        final Graphics2D g2d = (Graphics2D) image.getGraphics();
        if (scale) {
            g2d.scale(scalex, scaley);
        }
        g2d.translate(-bounds.x, -bounds.y);
        if (print) {
            c.printAll(g2d);
        } else {
            c.paintAll(g2d);
        }

        g2d.dispose();
        return image;
    }

    public static BufferedImage createCompatibleTranslucentImage(final int width,
                                                                 final int height) {
        return isHeadless() ? new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                : getGraphicsConfiguration().createCompatibleImage(width, height,
                                                                   Transparency.TRANSLUCENT);
    }

    private static boolean isHeadless() {
        return GraphicsEnvironment.isHeadless();
    }

    private static GraphicsConfiguration getGraphicsConfiguration() {
        return GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
    }
}
