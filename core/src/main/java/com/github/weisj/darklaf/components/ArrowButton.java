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
package com.github.weisj.darklaf.components;

import com.github.weisj.darklaf.icons.UIAwareIcon;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

import javax.swing.*;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.basic.BasicArrowButton;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public final class ArrowButton implements SwingConstants {


    private ArrowButton() {
    }


    public static JButton createUpDownArrow(final JComponent parent, final int orientation,
                                            final boolean center) {
        return createUpDownArrow(parent, orientation, center, false,
                                 new Insets(0, 0, 0, 0));
    }


    public static JButton createUpDownArrow(final JComponent parent, final int orientation,
                                            final boolean center, final boolean applyInsetsOnSize,
                                            final Insets insets) {
        UIAwareIcon icon;
        switch (orientation) {
            case NORTH:
                icon = (UIAwareIcon) UIManager.getIcon("ArrowButton.up.icon");
                break;
            case SOUTH:
                icon = (UIAwareIcon) UIManager.getIcon("ArrowButton.down.icon");
                break;
            default:
                throw new IllegalStateException("Invalid button orientation: " + orientation);
        }
        return createUpDownArrow(parent, icon, icon.getDual(), orientation, center, applyInsetsOnSize, insets);
    }


    public static JButton createUpDownArrow(final JComponent parent,
                                            final Icon activeIcon, final Icon inactiveIcon,
                                            final int orientation, final boolean center,
                                            final boolean applyInsetsOnSize, final Insets insets) {
        return new BasicArrowButton(orientation, null, null, null, null) {
            {
                putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
            }

            @Override
            public void paint(final Graphics g) {
                int x = (getWidth() - getIcon().getIconWidth()) / 2;
                int y = direction == NORTH ? getHeight() - getIcon().getIconHeight() + 4 : -4;
                if (center) {
                    y = (getHeight() - getIcon().getIconHeight()) / 2;
                }
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
                    return new DimensionUIResource(getIcon().getIconWidth() + insets.left + insets.right,
                                                   getIcon().getIconHeight() + insets.top + insets.bottom);
                }
            }

            @Override
            public void paintTriangle(final Graphics g, final int x, final int y,
                                      final int size, final int direction, final boolean isEnabled) {
                getIcon().paintIcon(this, g, x, y);
            }


            @Override
            public Insets getInsets() {
                return getInsets(new Insets(0, 0, 0, 0));
            }


            @Override
            public Insets getInsets(final Insets i) {
                i.left = insets.left;
                i.right = insets.right;
                i.top = insets.top;
                i.bottom = insets.bottom;
                return i;
            }


            @Override
            public boolean isOpaque() {
                return false;
            }
        };
    }
}
