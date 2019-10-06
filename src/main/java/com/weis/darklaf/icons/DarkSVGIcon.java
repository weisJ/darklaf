package com.weis.darklaf.icons;

import com.kitfox.svg.app.beans.SVGIcon;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.io.Serializable;
import java.net.URI;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkSVGIcon implements Icon, Serializable {

    private final Dimension size;
    private final SVGIcon icon;

    /**
     * Method to fetch the SVG icon from a url.
     *
     * @param uri           the uri from which to fetch the SVG icon.
     * @param displayWidth  display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(@NotNull final URI uri, final int displayWidth, final int displayHeight) {
        size = new Dimension(displayWidth, displayHeight);

        icon = new SVGIcon();
        icon.setSvgURI(uri);
        icon.setScaleToFit(true);
        icon.setAntiAlias(true);
    }

    @Contract(pure = true)
    private DarkSVGIcon(final int width, final int height, @NotNull final DarkSVGIcon icon) {
        this.size = new Dimension(width, height);
        this.icon = icon.icon;
    }

    public DarkSVGIcon derive(final int width, final int height) {
        return new DarkSVGIcon(width, height, this);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        paintIcon(c, g, x, y, 0);
    }

    /**
     * Paint the icon with rotation.
     *
     * @param c        the parent component.
     * @param g        the graphics object.
     * @param x        the x coordinate
     * @param y        the y coordinate
     * @param rotation the rotation in radians.
     */
    public void paintIcon(final Component c, @NotNull final Graphics g, final int x, final int y,
                          final double rotation) {
        var g2 = (Graphics2D) g.create();
        g2.translate(x, y);
        if (rotation != 0) {
            g2.setTransform(AffineTransform.getRotateInstance(rotation, size.width / 2.0,
                                                              size.height / 2.0));
        }
        icon.setPreferredSize(size);
        icon.paintIcon(c, g2, 0, 0);
        g2.dispose();
    }

    @Override
    public int getIconWidth() {
        return size.width;
    }

    @Override
    public int getIconHeight() {
        return size.height;
    }
}
