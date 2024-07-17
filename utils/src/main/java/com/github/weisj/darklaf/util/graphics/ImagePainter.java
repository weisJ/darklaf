/*
 * MIT License
 *
 * Copyright (c) 2021-2024 Jannis Weis
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
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;

public final class ImagePainter {
    /**
     * A hidpi-aware wrapper over {@link Graphics#drawImage(Image, int, int, ImageObserver)}.
     *
     * @see #drawImage(Graphics, Image, Rectangle, Rectangle, ImageObserver)
     */
    public static void drawImage(final Graphics g, final Image image, final int x, final int y,
            final ImageObserver observer) {
        drawImage(g, image, new Rectangle(x, y, -1, -1), null, null, observer);
    }

    static void drawImage(final Graphics g, final Image image, final int x, final int y, final int width,
            final int height, final BufferedImageOp op,
            final ImageObserver observer) {
        Rectangle srcBounds = width >= 0 && height >= 0 ? new Rectangle(x, y, width, height) : null;
        drawImage(g, image, new Rectangle(x, y, width, height), srcBounds, op, observer);
    }

    /**
     * A hidpi-aware wrapper over {@link Graphics#drawImage(Image, int, int, int, int, ImageObserver)}.
     *
     * @see #drawImage(Graphics, Image, Rectangle, Rectangle, BufferedImageOp, ImageObserver)
     */
    public static void drawImage(final Graphics g, final Image image, final Rectangle dstBounds,
            final ImageObserver observer) {
        drawImage(g, image, dstBounds, null, null, observer);
    }

    public static void drawImage(final Graphics g,
            final Image image,
            final Rectangle dstBounds,
            final Rectangle srcBounds,
            final ImageObserver observer) {
        drawImage(g, image, dstBounds, srcBounds, null, observer);
    }

    /**
     * A hidpi-aware wrapper over
     * {@link Graphics#drawImage(Image, int, int, int, int, int, int, int, int, ImageObserver)}.
     * <p>
     * The {@code dstBounds} and {@code srcBounds} are in the user space (just like the width/height of
     * the image). If {@code dstBounds} is null or if its width/height is set to (-1) the image bounds
     * or the image width/height is used. If {@code srcBounds} is null or if its width/height is set to
     * (-1) the image bounds or the image right/bottom area to the provided x/y is used.
     */
    public static void drawImage(Graphics g,
            Image image,
            final Rectangle dstBounds,
            final Rectangle srcBounds,
            final BufferedImageOp op,
            final ImageObserver observer) {
        int userWidth = image.getWidth(observer);
        int userHeight = image.getHeight(observer);

        int dx = 0;
        int dy = 0;
        int dw = -1;
        int dh = -1;
        if (dstBounds != null) {
            dx = dstBounds.x;
            dy = dstBounds.y;
            dw = dstBounds.width;
            dh = dstBounds.height;
        }
        boolean hasDstSize = dw >= 0 && dh >= 0;

        Graphics2D invG = null;
        double scaleX = 1;
        double scaleY = 1;
        if (image instanceof ScaledImage scaledImage) {
            Image delegate = scaledImage.getDelegate();
            if (delegate != null) image = delegate;
            scaleX = scaledImage.getScaleX();
            scaleY = scaledImage.getScaleY();

            userWidth = (int) (userWidth / scaleX);
            userHeight = (int) (userHeight / scaleY);

            AffineTransform tx = ((Graphics2D) g).getTransform();
            if (scaleX == tx.getScaleX() && scaleY == tx.getScaleY()) {
                // The image has the same original scale as the graphics scale. However, the real image
                // scale - userSize/realSize - can suffer from inaccuracy due to the image user size
                // rounding to int (userSize = (int)realSize/originalImageScale). This may case quality
                // loss if the image is drawn via Graphics.drawImage(image, <srcRect>, <dstRect>)
                // due to scaling in Graphics. To avoid that, the image should be drawn directly via
                // Graphics.drawImage(image, 0, 0) on the unscaled Graphics.
                double gScaleX = tx.getScaleX();
                double gScaleY = tx.getScaleY();
                tx.scale(1 / gScaleX, 1 / gScaleY);
                tx.translate(dx * gScaleX, dy * gScaleY);
                dx = dy = 0;
                g = invG = (Graphics2D) g.create();
                invG.setTransform(tx);
            }
        }
        try {
            if (op != null && image instanceof BufferedImage) {
                image = op.filter((BufferedImage) image, null);
            }
            if (invG != null && hasDstSize) {
                dw = (int) Math.round(dw * scaleX);
                dh = (int) Math.round(dh * scaleY);
            }
            if (srcBounds != null) {
                int sx = (int) Math.round(srcBounds.x * scaleX);
                int sy = (int) Math.round(srcBounds.y * scaleY);
                int sw = srcBounds.width >= 0
                        ? (int) Math.round(srcBounds.width * scaleX)
                        : (int) Math.round(userWidth * scaleX) - sx;
                int sh = srcBounds.height >= 0
                        ? (int) Math.round(srcBounds.height * scaleY)
                        : (int) Math.round(userHeight * scaleY) - sy;
                if (!hasDstSize) {
                    dw = (int) Math.round(userWidth * scaleX);
                    dh = (int) Math.round(userHeight * scaleY);
                }
                g.drawImage(image,
                        dx, dy, dx + dw, dy + dh,
                        sx, sy, sx + sw, sy + sh,
                        observer);
            } else if (hasDstSize) {
                g.drawImage(image, dx, dy, dw, dh, observer);
            } else if (invG == null) {
                g.drawImage(image, dx, dy, userWidth, userHeight, observer);
            } else {
                g.drawImage(image, dx, dy, observer);
            }
        } finally {
            if (invG != null) invG.dispose();
        }
    }
}
