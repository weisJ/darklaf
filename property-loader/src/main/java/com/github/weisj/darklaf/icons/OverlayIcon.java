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
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.util.Objects;

import javax.swing.*;

import com.github.weisj.darklaf.util.Alignment;

public class OverlayIcon implements Icon {

    private Icon icon;
    private Icon overlay;
    private Alignment alignment;

    public OverlayIcon(final Icon icon, final Icon overlay, final Alignment alignment) {
        setIcon(icon);
        setOverlay(overlay);
        setAlignment(alignment);
    }

    public Icon getIcon() {
        return icon;
    }

    public void setIcon(final Icon icon) {
        this.icon = icon != null ? icon : EmptyIcon.create(0);
    }

    public void setOverlay(final Icon overlay) {
        this.overlay = overlay != null ? overlay : EmptyIcon.create(0);
    }

    public void setAlignment(final Alignment alignment) {
        Objects.requireNonNull(alignment);
        this.alignment = alignment;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        icon.paintIcon(c, g, x, y);
        Point pos = alignment.alignInside(new Dimension(overlay.getIconWidth(), overlay.getIconHeight()),
                new Rectangle(x, y, getIconWidth(), getIconHeight()));
        overlay.paintIcon(c, g, pos.x, pos.y);
    }

    @Override
    public int getIconWidth() {
        return icon.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return icon.getIconHeight();
    }
}
