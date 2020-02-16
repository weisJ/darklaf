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
package com.github.weisj.darklaf.icons;

import com.github.weisj.darklaf.components.alignment.Alignment;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;

public class RotatableIcon implements Icon {

    private Icon icon;
    private Alignment alignment;


    public RotatableIcon() {
        this(null);
    }


    public RotatableIcon(final Icon icon) {
        setIcon(icon);
        this.alignment = null;
    }

    public void setIcon(final Icon icon) {
        this.icon = icon == null ? EmptyIcon.create(0) : icon;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        if (icon instanceof DarkSVGIcon) {
            ((DarkSVGIcon) icon).paintIcon(c, g, x, y, getAngle());
        } else if (icon != null) {
            Graphics2D g2 = (Graphics2D) g.create();
            AffineTransform transform = new AffineTransform();
            transform.rotate(getAngle(), x + getIconWidth() / 2.0, y + getIconHeight() / 2.0);
            g2.transform(transform);
            icon.paintIcon(c, g2, x, y);
        }
    }


    private double getAngle() {
        double angle = 0.0;
        switch (alignment) {
            case NORTH:
            case CENTER:
                angle = 0.0;
                break;
            case SOUTH:
                angle = 180.0;
                break;
            case EAST:
                angle = 90.0;
                break;
            case WEST:
                angle = 270.0;
                break;
            case NORTH_EAST:
                angle = 45.0;
                break;
            case NORTH_WEST:
                angle = 315.0;
                break;
            case SOUTH_EAST:
                angle = 135.0;
                break;
            case SOUTH_WEST:
                angle = 225.0;
                break;
        }
        return Math.toRadians(angle);
    }

    @Override
    public int getIconWidth() {
        return icon.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return icon.getIconHeight();
    }

    public Alignment getOrientation() {
        return alignment;
    }

    public void setOrientation(final Alignment alignment) {
        this.alignment = alignment;
    }
}
