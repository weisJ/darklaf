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
package com.github.weisj.darklaf.graphics;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.lang.reflect.Field;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import com.github.weisj.darklaf.compatibility.SwingUtil;
import com.github.weisj.darklaf.ui.OpacityBufferedUI;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.*;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public final class StringPainter {

    private static final Logger LOGGER = LogUtil.getLogger(StringPainter.class);

    private static boolean translucentAAPaintingEnabled = true;
    private static boolean experimentalAntialiasingEnabled = false;

    public static void setExperimentalAntialiasingEnabled(final boolean enabled) {
        experimentalAntialiasingEnabled = enabled;
    }

    public static void setTranslucentAAPaintingEnabled(final boolean enabled) {
        translucentAAPaintingEnabled = enabled;
    }

    public static boolean isTranslucentAAPaintingEnabled() {
        return translucentAAPaintingEnabled;
    }

    public static boolean isExperimentalAntialiasingEnabled() {
        return experimentalAntialiasingEnabled;
    }

    public static <T extends JComponent> void drawString(final Graphics g, final T c, final String text,
            final Rectangle textRect) {
        drawString(g, c, text, textRect, SwingUtil.getFontMetrics(c, g));
    }

    public static <T extends JComponent> void drawString(final Graphics g, final T c, final View view,
            final String text, final Rectangle textRect, final FontMetrics fm) {
        drawStringImpl(g, c, view, text, textRect, c.getFont(), fm, -1);
    }

    public static <T extends JComponent> void drawString(final Graphics g, final T c, final String text,
            final Rectangle textRect, final FontMetrics fm) {
        drawStringImpl(g, c, null, text, textRect, c.getFont(), fm, -1);
    }

    public static <T extends JComponent> void drawStringUnderlineCharAt(final Graphics g, final T c, final String text,
            final int mnemIndex, final Rectangle textRect) {
        drawStringUnderlineCharAt(g, c, text, mnemIndex, textRect, c.getFont());
    }

    public static <T extends JComponent> void drawStringUnderlineCharAt(final Graphics g, final T c, final String text,
            final int mnemIndex, final Rectangle textRect, final Font f) {
        drawStringUnderlineCharAt(g, c, text, mnemIndex, textRect, f, SwingUtil.getFontMetrics(c, g));
    }

    public static <T extends JComponent> void drawStringUnderlineCharAt(final Graphics g, final T c, final View view,
            final String text, final int mnemIndex, final Rectangle textRect, final Font font, final FontMetrics fm) {
        drawStringImpl(g, c, view, text, textRect, font, fm, mnemIndex);
    }

    public static <T extends JComponent> void drawStringUnderlineCharAt(final Graphics g, final T c, final String text,
            final int mnemIndex, final Rectangle textRect, final Font font, final FontMetrics fm) {
        drawStringImpl(g, c, null, text, textRect, font, fm, mnemIndex);
    }

    public static <T extends JComponent> void drawStringImpl(final Graphics g, final T c, final View view,
            final String text, final Rectangle textRect, final Font font, final FontMetrics fm, final int mnemIndex) {
        drawStringImpl(g, c, view, text, textRect, font, fm, mnemIndex, c.getBackground());
    }

    public static <T extends JComponent> void drawStringImpl(final Graphics g, final T c, final View view,
            final String text, final Rectangle textRect, final Font font, final FontMetrics fm, final int mnemIndex,
            final Color background) {
        if (text == null || text.equals("")) return;

        GraphicsContext context = GraphicsUtil.setupAntialiasing(g);

        final int asc = fm.getAscent();
        final int x = textRect.x;
        final int y = textRect.y;

        Graphics2D drawingGraphics = (Graphics2D) g;
        BufferedImage img = null; // Only needed for translucent AA painting.
        Point textPos = null; // Only needed for experimental algorithm.

        Color fgColor = g.getColor();
        Color bgColor = background;

        /*
         * If there is a non-opaque parent on Windows no sub-pixel AA is supported. In this case we paint
         * the text to an offscreen image with opaque background and paste it draw it back to the original
         * graphics object.
         *
         * See https://bugs.openjdk.java.net/browse/JDK-8215980?attachmentOrder=desc
         */
        Component window = getNonOpaqueWindow(c);
        boolean paintOpaqueBuffered = window != null;

        if (paintOpaqueBuffered) {
            LOGGER.finest(() -> "Using opaque buffering for " + c);
            double scaleX = Scale.getScaleX((Graphics2D) g);
            double scaleY = Scale.getScaleX((Graphics2D) g);

            if (experimentalAntialiasingEnabled) {
                textPos = new Point(x, y);
                textPos.setLocation(SwingUtilities.convertPoint(c, textPos, window));
                textPos.setLocation((int) Math.round(scaleX * textPos.x), (int) Math.round(scaleX * textPos.y));

                /*
                 * Ensure the background color has sufficient contrast to the foreground.
                 */
                Color fg = g.getColor();
                double brightness = ColorUtil.getPerceivedBrightness(fg);
                bgColor = brightness > 127 ? Color.BLACK : Color.WHITE;
            }

            img = ImageUtil.createCompatibleImage((int) Math.round(scaleX * textRect.width),
                    (int) Math.round(scaleY * textRect.height));
            drawingGraphics = prepareImage(img, bgColor, fgColor, scaleX, scaleY);
            textRect.setLocation(0, 0);
        }
        drawingGraphics.setFont(font);

        View v = view != null ? view : PropertyUtil.getObject(c, BasicHTML.propertyKey, View.class);
        if (v != null) {
            v.paint(drawingGraphics, textRect);
        } else {
            int textY = textRect.y + asc;
            if (mnemIndex >= 0) {
                SwingUtil.drawStringUnderlineCharAt(c, drawingGraphics, text, mnemIndex, textRect.x, textY);
            } else {
                SwingUtil.drawString(c, drawingGraphics, text, textRect.x, textY);
            }
        }

        if (paintOpaqueBuffered) {
            drawingGraphics.dispose();
            Image result = postProcessImage((Graphics2D) g, img, textPos, bgColor, fgColor);
            g.drawImage(result, x, y, textRect.width, textRect.height, null);
        }
        context.restore();
    }

    private static Image postProcessImage(final Graphics2D g, final BufferedImage img, final Point textPos,
            final Color bgColor, final Color fgColor) {
        if (experimentalAntialiasingEnabled) {
            final BufferedImage destImg = getImage(g);
            ImageFilter filter = new AntialiasingImageFilter(destImg, textPos.x, textPos.y, fgColor, bgColor);
            return Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(img.getSource(), filter));
        } else {
            return img;
        }
    }

    private static Component getNonOpaqueWindow(final JComponent c) {
        boolean imgGraphics = false;
        Component window = c;
        if (translucentAAPaintingEnabled && SystemInfo.isWindows && !GraphicsUtil.isOpaqueBuffered(c)) {
            Component comp = c;
            while (comp != null) {
                Color bg = comp.getBackground();
                imgGraphics = bg != null && bg.getAlpha() < 255;
                if (imgGraphics) {
                    break;
                }
                comp = comp.getParent();
            }
            if (imgGraphics) {
                window = DarkUIUtil.getWindow(comp);
            }
            return imgGraphics ? window : null;
        }
        return null;
    }

    private static BufferedImage getImage(final Graphics2D graphics2D) {
        try {
            Field surfaceField = graphics2D.getClass().getField("surfaceData");
            Object surfaceDataValue = surfaceField.get(graphics2D);
            if (surfaceDataValue == null) return null;

            Field imgField;
            try {
                imgField = surfaceDataValue.getClass().getDeclaredField("bufImg"); // BufImgSurfaceData
            } catch (final Exception ignored) {
                imgField = surfaceDataValue.getClass().getField("offscreenImage"); // CGLSurfaceData
            }
            imgField.setAccessible(true);
            Object img = imgField.get(surfaceDataValue);
            if (img instanceof BufferedImage) {
                return (BufferedImage) img;
            }
        } catch (final Exception e) {
            LOGGER.log(Level.SEVERE, "Couldn't retrieve Graphics backing image", e);
        }
        return null;
    }

    private static Graphics2D prepareImage(final BufferedImage img, final Color background, final Color color,
            final double xScale, final double yScale) {
        Graphics2D g = (Graphics2D) img.getGraphics();
        g.setColor(background);
        g.fillRect(0, 0, img.getWidth(), img.getHeight());
        g.setColor(color);
        g.setClip(0, 0, img.getWidth(), img.getHeight());
        g.scale(xScale, yScale);
        return g;
    }

    public static void paintOpacityBufferedUI(final Graphics g, final JComponent c, final OpacityBufferedUI ui) {
        boolean opaqueBuffered = translucentAAPaintingEnabled && GraphicsUtil.isOpaqueBuffered(c);
        if (opaqueBuffered) {
            paintOpacityBuffered(g, c, ui);
        } else {
            ui.updateUI(g, c);
        }
    }

    public static void paintOpacityBuffered(final Graphics g, final JComponent c, final OpacityBufferedUI ui) {
        double scaleX = Scale.getScaleX((Graphics2D) g);
        double scaleY = Scale.getScaleX((Graphics2D) g);
        BufferedImage img = ImageUtil.createCompatibleImage((int) Math.round(scaleX * c.getWidth()),
                (int) Math.round(scaleY * c.getHeight()));
        Graphics imgGraphics = img.getGraphics();
        imgGraphics.setColor(c.getBackground());
        imgGraphics.fillRect(0, 0, c.getWidth(), c.getHeight());
        imgGraphics.setClip(0, 0, img.getWidth(), img.getHeight());
        ((Graphics2D) imgGraphics).scale(scaleX, scaleY);
        ui.updateUI(imgGraphics, c);
        imgGraphics.dispose();
        g.drawImage(img, 0, 0, c.getWidth(), c.getHeight(), null);
    }
}
