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
package com.github.weisj.darklaf.graphics;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.util.function.BiConsumer;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.icons.ImageSource;
import com.github.weisj.darklaf.icons.ThemedIcon;
import com.github.weisj.darklaf.icons.UIAwareIcon;
import com.github.weisj.darklaf.theme.event.ThemeInstalledListener;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.Scale;

/**
 * @author Jannis Weis
 */
public final class ImageUtil {

    private ImageUtil() {}

    private static final int FRAME_ICON_SIZE = 32;

    public static Image createFrameIcon(final Icon icon, final Window c) {
        if (c instanceof JFrame) {
            return createFrameIcon(icon, (JFrame) c);
        } else if (c instanceof JDialog) {
            return createFrameIcon(icon, (JDialog) c);
        } else {
            return createScaledFrameIcon(icon, c);
        }
    }

    public static Image createFrameIcon(final Icon icon, final JFrame c) {
        return createWindowIcon(icon, c, JFrame::setIconImage);
    }

    public static Image createFrameIcon(final Icon icon, final JDialog c) {
        return createWindowIcon(icon, c, JDialog::setIconImage);
    }

    private static <T extends Window> Image createWindowIcon(final Icon icon, final T c,
                                                             final BiConsumer<T, Image> iconSetter) {
        if (icon == null) return null;
        if (c != null) {
            if (iconNeedUpdates(icon)) {
                ThemeInstalledListener listener = e -> iconSetter.accept(c, iconToImage(icon, c));
                LafManager.addThemeChangeListener(listener);
            }
            PropertyChangeListener propertyChangeListener = e -> iconSetter.accept(c, iconToImage(icon, c));
            c.addPropertyChangeListener(PropertyKey.GRAPHICS_CONFIGURATION, propertyChangeListener);
        }
        return createScaledFrameIcon(icon, c);
    }

    private static boolean iconNeedUpdates(final Icon icon) {
        return icon instanceof UIAwareIcon || icon instanceof ThemedIcon;
    }

    private static Image createScaledFrameIcon(final Icon icon, final Window c) {
        if (c != null && !c.isVisible()) {
            Component parent = c.getParent();
            if (parent != null) {
                return iconToImage(icon, c);
            }
        }
        return iconToImage(icon, c);
    }

    public static Image iconToImage(final Icon icon, final Component c) {
        if (icon == null) return null;
        int w = icon.getIconWidth();
        int h = icon.getIconHeight();
        GraphicsConfiguration gc = c.getGraphicsConfiguration();
        double sx = Scale.getScaleX(gc);
        double sy = Scale.getScaleY(gc);
        double scaleX = sx * (((double) FRAME_ICON_SIZE) / w);
        double scaleY = sy * (((double) FRAME_ICON_SIZE) / h);
        return createScaledImage(icon, scaleX, scaleY);
    }

    public static Image createScaledImage(final Icon icon, final double scalex, final double scaley) {
        if (icon == null) return null;
        int w = (int) (scalex * icon.getIconWidth());
        int h = (int) (scaley * icon.getIconHeight());
        if (icon instanceof ImageSource) {
            return ((ImageSource) icon).createImage(w, h);
        } else {
            BufferedImage image = createCompatibleTranslucentImage(w, h);
            Graphics2D g = (Graphics2D) image.getGraphics();
            g.scale(scalex, scaley);
            icon.paintIcon(null, g, 0, 0);
            g.dispose();
            return image;
        }
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
        GraphicsConfiguration gc = c.getGraphicsConfiguration();
        return scaledImageFromComponent(c, bounds, Scale.getScaleX(gc), Scale.getScaleY(gc), true);
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
            image = createCompatibleTranslucentImage((int) (scalex * bounds.width), (int) (scaley * bounds.height));
        } else {
            image = createCompatibleTranslucentImage(bounds.width, bounds.height);
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

    public static BufferedImage createCompatibleImage(final int width,
                                                      final int height) {
        return isHeadless() ? new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
                : getGraphicsConfiguration().createCompatibleImage(width, height,
                                                                   Transparency.OPAQUE);
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
