package com.weis.darklaf.icons;

import com.kitfox.svg.app.beans.SVGIcon;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.net.URI;

/**
 * Icon from SVG image.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkSVGIcon implements Icon {

    private final int displayWidth;
    private final int displayHeight;
    private final SVGIcon icon;

    /**
     * Method to fetch the SVG icon from a url.
     *
     * @param uri           the uri from which to fetch the SVG icon.
     * @param displayWidth  display width of icon.
     * @param displayHeight display height of icon.
     */
    public DarkSVGIcon(@NotNull final URI uri, final int displayWidth, final int displayHeight) {
        this.displayHeight = displayHeight;
        this.displayWidth = displayWidth;

        icon = new SVGIcon();
        icon.setSvgURI(uri);
        icon.setScaleToFit(true);
        icon.setPreferredSize(new Dimension(displayWidth, displayHeight));
        icon.setAntiAlias(true);
    }

    private void renderIcon(@NotNull final Graphics2D gc, final Component c,
                            final double width, final double height, final double angleRadians) {
        if (angleRadians != 0) {
            gc.setTransform(AffineTransform.getRotateInstance(angleRadians, width / 2, height / 2));
        }
        icon.paintIcon(c, gc, 0, 0);
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
        renderIcon(g2, c, displayWidth, displayHeight, rotation);
        g2.dispose();
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        paintIcon(c, g, x, y, 0);
    }

    @Override
    public int getIconWidth() {
        return displayWidth;
    }

    @Override
    public int getIconHeight() {
        return displayHeight;
    }
}
