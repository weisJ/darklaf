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
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;
import java.awt.geom.AffineTransform;

import javax.swing.*;

import com.github.weisj.darklaf.util.Alignment;

public class RotatableIcon implements Icon {

    private Icon icon;
    private Alignment alignment;
    private double angle;

    public RotatableIcon() {
        this(null);
    }

    public RotatableIcon(final Icon icon) {
        setIcon(icon);
    }

    public void setIcon(final Icon icon) {
        this.icon = icon == null ? EmptyIcon.create(0) : icon;
    }

    public Icon getIcon() {
        return icon;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        if (icon instanceof RotateIcon) {
            ((RotateIcon) icon).paintIcon(c, g, x, y, getAngle());
        } else if (icon != null) {
            Graphics2D g2 = (Graphics2D) g.create();
            AffineTransform transform = new AffineTransform();
            transform.rotate(getAngle(), x + getIconWidth() / 2.0, y + getIconHeight() / 2.0);
            g2.transform(transform);
            icon.paintIcon(c, g2, x, y);
        }
    }

    public double getAngle() {
        return angle;
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
        this.alignment = alignment != null ? alignment : Alignment.NORTH;
        this.angle = this.alignment.getAngle();
    }

    public void setRotation(final double angle) {
        this.alignment = null;
        this.angle = angle;
    }
}
