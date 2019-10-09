package com.weis.darklaf.ui.toolbar;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkToolBarBorder extends AbstractBorder implements UIResource, SwingConstants {

    public void paintBorder(final Component c, @NotNull final Graphics g,
                            final int x, final int y, final int w, final int h) {
        g.translate(x, y);

        if (isFloatable(c)) {
            if (((JToolBar) c).getOrientation() == HORIZONTAL) {
                var icon = getHorizontalGrip();
                int yIcon = h / 2 - icon.getIconHeight() / 2;
                if (c.getComponentOrientation().isLeftToRight()) {
                    icon.paintIcon(c, g, 0, yIcon);
                } else {
                    icon.paintIcon(c, g, w - icon.getIconWidth(), yIcon);
                }
            } else {
                var icon = getVerticalGrip();
                int xIcon = w / 2 - icon.getIconWidth() / 2;
                icon.paintIcon(c, g, xIcon, 0);
            }
        }
        g.translate(-x, -y);
    }

    @Contract("null -> false")
    private boolean isFloatable(final Component c) {
        return c instanceof JToolBar && ((JToolBar) c).isFloatable();
    }

    protected Icon getHorizontalGrip() {
        return UIManager.getIcon("ToolBar.horizontalGrip.icon");
    }

    protected Icon getVerticalGrip() {
        return UIManager.getIcon("ToolBar.verticalGrip.icon");
    }

    public Insets getBorderInsets(final Component c, final Insets newInsets) {
        if (isFloatable(c)) {
            if (((JToolBar) c).getOrientation() == HORIZONTAL) {
                var icon = getHorizontalGrip();
                if (c.getComponentOrientation().isLeftToRight()) {
                    newInsets.left = icon.getIconWidth();
                } else {
                    newInsets.right = icon.getIconWidth();
                }
            } else {
                newInsets.top = getVerticalGrip().getIconHeight();
            }
        }

        if (c instanceof JToolBar) {
            Insets margin = ((JToolBar) c).getMargin();
            if (margin != null) {
                newInsets.left += margin.left;
                newInsets.top += margin.top;
                newInsets.right += margin.right;
                newInsets.bottom += margin.bottom;
            }
        }
        return newInsets;
    }
}