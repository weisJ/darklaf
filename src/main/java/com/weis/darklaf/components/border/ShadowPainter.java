package com.weis.darklaf.components.border;

// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be
// found in the LICENSE file.

import com.weis.darklaf.util.GraphicsUtil;
import com.weis.darklaf.util.ImageUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import static com.weis.darklaf.util.GraphicsUtil.SCALE;

/**
 * @author Konstantin Bulenkov
 */
public class ShadowPainter {
    protected final Icon myTop;
    protected final Icon myTopRight;
    protected final Icon myRight;
    protected final Icon myBottomRight;
    protected final Icon myBottom;
    protected final Icon myBottomLeft;
    protected final Icon myLeft;
    protected final Icon myTopLeft;

    private Icon myCroppedTop = null;
    private Icon myCroppedRight = null;
    private Icon myCroppedBottom = null;
    private Icon myCroppedLeft = null;

    @Nullable
    private Color myBorderColor;

    public ShadowPainter(final Icon top, final Icon topRight, final Icon right,
                         final Icon bottomRight, final Icon bottom, final Icon bottomLeft,
                         final Icon left, final Icon topLeft) {
        myTop = top;
        myTopRight = topRight;
        myRight = right;
        myBottomRight = bottomRight;
        myBottom = bottom;
        myBottomLeft = bottomLeft;
        myLeft = left;
        myTopLeft = topLeft;
        updateIcons();
    }

    @Contract(pure = true)
    public ShadowPainter(final Icon top, final Icon topRight, final Icon right,
                         final Icon bottomRight, final Icon bottom, final Icon bottomLeft,
                         final Icon left, final Icon topLeft, @Nullable final Color borderColor) {
        this(top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft);
        myBorderColor = borderColor;
    }

    private static void fill(final Graphics g, final Icon pattern, final int x, final int y, final int from,
                             final int to, final boolean horizontally) {
        double scale = SCALE;
        if (GraphicsUtil.isHighDpiEnabled() && Math.ceil(scale) > scale) {
            // Direct painting for fractional scale
            BufferedImage img = ImageUtil.toBufferedImage(ImageUtil.toImage(pattern));
            int patternSize = horizontally ? img.getWidth() : img.getHeight();
            Graphics2D g2d = (Graphics2D) g.create();
            try {
                g2d.scale(1 / scale, 1 / scale);
                g2d.translate(x * scale, y * scale);
                for (int at = (int) Math.floor(from * scale); at < to * scale; at += patternSize) {
                    if (horizontally) {
                        g2d.drawImage(img, at, 0, null);
                    } else {
                        g2d.drawImage(img, 0, at, null);
                    }
                }
            } finally {
                g2d.dispose();
            }
        } else {
            for (int at = from; at < to; at++) {
                if (horizontally) {
                    pattern.paintIcon(null, g, x + at, y);
                } else {
                    pattern.paintIcon(null, g, x, y + at);
                }
            }
        }
    }

    public void setBorderColor(@Nullable final Color borderColor) {
        myBorderColor = borderColor;
    }

    private void updateIcons() {
        myCroppedTop = ImageUtil.cropIcon(myTop, 1, Integer.MAX_VALUE);
        myCroppedRight = ImageUtil.cropIcon(myRight, Integer.MAX_VALUE, 1);
        myCroppedBottom = ImageUtil.cropIcon(myBottom, 1, Integer.MAX_VALUE);
        myCroppedLeft = ImageUtil.cropIcon(myLeft, Integer.MAX_VALUE, 1);
    }

    public BufferedImage createShadow(@NotNull final JComponent c, final int width, final int height) {
        final BufferedImage image = c.getGraphicsConfiguration()
                                     .createCompatibleImage(width, height, Transparency.TRANSLUCENT);
        final Graphics2D g = image.createGraphics();
        paintShadow(c, g, 0, 0, width, height);
        g.dispose();
        return image;
    }

    public void paintShadow(final Component c, final Graphics2D g, final int x, final int y,
                            final int width, final int height) {
        final int leftSize = myCroppedLeft.getIconWidth();
        final int rightSize = myCroppedRight.getIconWidth();
        final int bottomSize = myCroppedBottom.getIconHeight();
        final int topSize = myCroppedTop.getIconHeight();
//        updateIcons();

        int delta = myTopLeft.getIconHeight() + myBottomLeft.getIconHeight() - height;
        if (delta > 0) { // Corner icons are overlapping. Need to handle this
            Shape clip = g.getClip();

            int topHeight = myTopLeft.getIconHeight() - delta / 2;
            Area top = new Area(new Rectangle2D.Float(x, y, width, topHeight));
            if (clip != null) {
                top.intersect(new Area(clip));
            }
            g.setClip(top);

            myTopLeft.paintIcon(c, g, x, y);
            myTopRight.paintIcon(c, g, x + width - myTopRight.getIconWidth(), y);

            int bottomHeight = myBottomLeft.getIconHeight() - delta + delta / 2;
            Area bottom = new Area(new Rectangle2D.Float(x, y + topHeight, width, bottomHeight));
            if (clip != null) {
                bottom.intersect(new Area(clip));
            }
            g.setClip(bottom);

            myBottomLeft.paintIcon(c, g, x, y + height - myBottomLeft.getIconHeight());
            myBottomRight.paintIcon(c, g, x + width - myBottomRight.getIconWidth(),
                                    y + height - myBottomRight.getIconHeight());

            g.setClip(clip);
        } else {
            myTopLeft.paintIcon(c, g, x, y);
            myTopRight.paintIcon(c, g, x + width - myTopRight.getIconWidth(), y);
            myBottomLeft.paintIcon(c, g, x, y + height - myBottomLeft.getIconHeight());
            myBottomRight.paintIcon(c, g, x + width - myBottomRight.getIconWidth(),
                                    y + height - myBottomRight.getIconHeight());
        }

        fill(g, myCroppedTop, x, y, myTopLeft.getIconWidth(),
             width - myTopRight.getIconWidth(), true);
        fill(g, myCroppedBottom, x, y + height - bottomSize, myBottomLeft.getIconWidth(),
             width - myBottomRight.getIconWidth(), true);
        fill(g, myCroppedLeft, x, y, myTopLeft.getIconHeight(),
             height - myBottomLeft.getIconHeight(), false);
        fill(g, myCroppedRight, x + width - rightSize, y, myTopRight.getIconHeight(),
             height - myBottomRight.getIconHeight(), false);

        if (myBorderColor != null) {
            g.setColor(myBorderColor);
            g.drawRect(x + leftSize - 1, y + topSize - 1, width - leftSize - rightSize + 1,
                       height - topSize - bottomSize + 1);
        }
    }
}
