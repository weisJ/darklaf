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
package com.github.weisj.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * @author Jannis Weis
 */
public final class ImageUtil {

    @Contract(pure = true)
    private ImageUtil() {
    }

    /**
     * Create image from component.
     *
     * @param c      the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    @NotNull
    public static Image scaledImageFromComponent(@NotNull final Component c, @NotNull final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, Scale.SCALE_X, Scale.SCALE_Y);
    }

    /**
     * Create image from component.
     *
     * @param c      the component.
     * @param bounds the bounds inside the component to capture.
     * @param scalex the x scale
     * @param scaley the y scale
     * @return image containing the captured area.
     */
    @NotNull
    public static Image scaledImageFromComponent(@NotNull final Component c, @NotNull final Rectangle bounds,
                                                 final double scalex, final double scaley) {
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
        c.printAll(g2d);

        g2d.dispose();
        return image;
    }

    /**
     * Create image from component.
     *
     * @param c      the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    @NotNull
    public static Image imageFromComponent(@NotNull final Component c, @NotNull final Rectangle bounds) {
        return scaledImageFromComponent(c, bounds, 1.0, 1.0);
    }

    @NotNull
    public static Icon cropIcon(@NotNull final Icon icon, int maxWidth, int maxHeight) {
        if (icon.getIconHeight() <= maxHeight && icon.getIconWidth() <= maxWidth) {
            return icon;
        }

        Image image = toImage(icon);
        if (image == null) return icon;

        double scale = 1f;
        BufferedImage bi = ImageUtil.toBufferedImage(image);
        final Graphics2D g = bi.createGraphics();

        int imageWidth = image.getWidth(null);
        int imageHeight = image.getHeight(null);

        maxWidth = maxWidth == Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) Math.round(maxWidth * scale);
        maxHeight = maxHeight == Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) Math.round(maxHeight * scale);
        final int w = Math.min(imageWidth, maxWidth);
        final int h = Math.min(imageHeight, maxHeight);

        final BufferedImage img = new BufferedImage(w, h, Transparency.TRANSLUCENT);
        final int offX = imageWidth > maxWidth ? (imageWidth - maxWidth) / 2 : 0;
        final int offY = imageHeight > maxHeight ? (imageHeight - maxHeight) / 2 : 0;
        for (int col = 0; col < w; col++) {
            for (int row = 0; row < h; row++) {
                img.setRGB(col, row, bi.getRGB(col + offX, row + offY));
            }
        }
        g.dispose();
        return new ImageIcon(img);
    }

    public static Image toImage(final Icon icon) {
        if (icon instanceof ImageIcon) {
            return ((ImageIcon) icon).getImage();
        } else {
            BufferedImage image = new BufferedImage(icon.getIconWidth(), icon.getIconHeight(),
                                                    BufferedImage.TYPE_INT_RGB);
            icon.paintIcon(null, image.getGraphics(), 0, 0);
            return image;
        }
    }

    @NotNull
    @Contract("null -> fail")
    public static BufferedImage toBufferedImage(final Image image) {
        if (image == null) {
            throw new NullPointerException("Can't covert null image");
        }
        if (image instanceof BufferedImage) {
            return (BufferedImage) image;
        } else {
            BufferedImage bufferedImage = new BufferedImage(image.getWidth(null),
                                                            image.getHeight(null),
                                                            BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = bufferedImage.createGraphics();
            g.drawImage(image, 0, 0, null);
            g.dispose();
            return bufferedImage;
        }
    }

    @NotNull
    @Contract("_, _, _ -> new")
    public static BufferedImage createImage(final int width, final int height, final int type) {
        return new BufferedImage(Scale.scaleWidth(width), Scale.scaleHeight(height), type) {
            @Override
            public Graphics2D createGraphics() {
                Graphics2D g = super.createGraphics();
                g.scale(Scale.SCALE_X, Scale.SCALE_Y);
                return g;
            }
        };
    }

    public static BufferedImage createCompatibleTranslucentImage(final int width,
                                                                 final int height) {
        return isHeadless() ?
               new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB) :
               getGraphicsConfiguration().createCompatibleImage(width, height,
                                                                Transparency.TRANSLUCENT);
    }

    private static boolean isHeadless() {
        return GraphicsEnvironment.isHeadless();
    }

    private static GraphicsConfiguration getGraphicsConfiguration() {
        return GraphicsEnvironment.getLocalGraphicsEnvironment().
                getDefaultScreenDevice().getDefaultConfiguration();
    }
}
