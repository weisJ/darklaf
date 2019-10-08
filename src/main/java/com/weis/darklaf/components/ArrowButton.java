package com.weis.darklaf.components;

import com.weis.darklaf.icons.UIAwareIcon;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.basic.BasicArrowButton;
import java.awt.*;

public final class ArrowButton implements SwingConstants {

    @Contract(pure = true)
    private ArrowButton() {
    }

    @NotNull
    public static JButton createUpDownArrow(final JComponent parent, final int orientation,
                                            final boolean center) {
        return createUpDownArrow(parent, orientation, center, false,
                                 new Insets(0, 0, 0, 0));
    }

    @NotNull
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
        return createUpDownArrow(parent, icon, orientation, center, applyInsetsOnSize, insets);
    }

    @Contract("_, _, _, _, _, _ -> new")
    @NotNull
    public static JButton createUpDownArrow(final JComponent parent, final UIAwareIcon icon, final int orientation,
                                            final boolean center, final boolean applyInsetsOnSize,
                                            final Insets insets) {
        return new BasicArrowButton(orientation, null, null, null, null) {
            @Override
            public void paint(final Graphics g) {
                int x = (getWidth() - icon.getIconWidth()) / 2;
                int y = direction == NORTH ? getHeight() - icon.getIconHeight() + 4 : -4;
                if (center) {
                    y = (getHeight() - icon.getIconHeight()) / 2;
                }
                paintTriangle(g, x, y, 0, direction, parent.isEnabled());
            }

            @Override
            public Dimension getPreferredSize() {
                if (!applyInsetsOnSize) {
                    return new DimensionUIResource(icon.getIconWidth(), icon.getIconHeight());
                } else {
                    return new DimensionUIResource(icon.getIconWidth() + insets.left + insets.right,
                                                   icon.getIconHeight() + insets.top + insets.bottom);
                }
            }

            @Override
            public void paintTriangle(final Graphics g, final int x, final int y,
                                      final int size, final int direction, final boolean isEnabled) {
                if (isEnabled) {
                    icon.paintIcon(parent, g, x, y);
                } else {
                    icon.getDual().paintIcon(parent, g, x, y);
                }
            }

            @NotNull
            @Override
            public Insets getInsets() {
                return getInsets(new Insets(0, 0, 0, 0));
            }

            @NotNull
            @Contract("_ -> param1")
            @Override
            public Insets getInsets(final Insets i) {
                i.left = insets.left;
                i.right = insets.right;
                i.top = insets.top;
                i.bottom = insets.bottom;
                return i;
            }

            @Contract(pure = true)
            @Override
            public boolean isOpaque() {
                return false;
            }
        };
    }
}
