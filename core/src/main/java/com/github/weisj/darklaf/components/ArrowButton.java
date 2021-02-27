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
package com.github.weisj.darklaf.components;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.basic.BasicArrowButton;

import com.github.weisj.darklaf.ui.button.ButtonConstants;

/** @author Jannis Weis */
public final class ArrowButton implements SwingConstants {

    private ArrowButton() {}

    public static JButton createUpDownArrow(final JComponent parent, final int orientation, final boolean center) {
        return createUpDownArrow(parent, orientation, center, false, new Insets(0, 0, 0, 0));
    }

    public static JButton createUpDownArrow(final JComponent parent, final int orientation, final boolean center,
            final boolean applyInsetsOnSize, final Insets insets) {
        Icon icon;
        switch (orientation) {
            case NORTH:
                icon = UIManager.getIcon("ArrowButton.up.icon");
                break;
            case SOUTH:
                icon = UIManager.getIcon("ArrowButton.down.icon");
                break;
            default:
                throw new IllegalStateException("Invalid button orientation: " + orientation);
        }
        return createUpDownArrow(parent, icon, icon, orientation, center, applyInsetsOnSize, insets);
    }

    public static JButton createUpDownArrow(final JComponent parent, final Icon activeIcon, final Icon inactiveIcon,
            final int orientation, final boolean center, final boolean applyInsetsOnSize, final Insets insets) {
        return new BasicArrowButton(orientation, null, null, null, null) {
            private final Insets ins = insets != null ? insets : new Insets(0, 0, 0, 0);
            {
                putClientProperty(ButtonConstants.KEY_NO_BORDERLESS_OVERWRITE, true);
                setMargin(new Insets(0, 0, 0, 0));
                setBorder(BorderFactory.createEmptyBorder());
            }

            @Override
            public void paint(final Graphics g) {
                Insets margin = getMargin();
                int w = getWidth() - margin.left - margin.right;
                int h = getHeight() - margin.top - margin.bottom;
                int x = margin.left + (w - getIcon().getIconWidth()) / 2;
                int y;
                if (center) {
                    y = (h - getIcon().getIconHeight()) / 2;
                } else {
                    y = direction == NORTH ? h - getIcon().getIconHeight() + 4 : -4;
                }
                y += margin.top;
                paintTriangle(g, x, y, 0, direction, parent.isEnabled());
            }

            public Icon getIcon() {
                return isEnabled() ? activeIcon : inactiveIcon;
            }

            @Override
            public Dimension getPreferredSize() {
                if (!applyInsetsOnSize) {
                    return new DimensionUIResource(getIcon().getIconWidth(), getIcon().getIconHeight());
                } else {
                    Insets i = getInsets();
                    return new DimensionUIResource(getIcon().getIconWidth() + i.left + i.right,
                            getIcon().getIconHeight() + i.top + i.bottom);
                }
            }

            @Override
            public void paintTriangle(final Graphics g, final int x, final int y, final int size, final int direction,
                    final boolean isEnabled) {
                getIcon().paintIcon(this, g, x, y);
            }

            @Override
            public Insets getInsets() {
                return getInsets(new Insets(0, 0, 0, 0));
            }

            @Override
            public Insets getInsets(final Insets i) {
                i.left = ins.left;
                i.right = ins.right;
                i.top = ins.top;
                i.bottom = ins.bottom;
                return i;
            }

            @Override
            public boolean isOpaque() {
                return false;
            }
        };
    }
}
