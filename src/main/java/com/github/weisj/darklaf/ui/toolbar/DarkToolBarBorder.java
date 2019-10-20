/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.ui.toolbar;

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

    protected Icon horizontalGrip;
    protected Icon verticalGrip;

    public DarkToolBarBorder() {
        horizontalGrip = UIManager.getIcon("ToolBar.horizontalGrip.icon");
        verticalGrip = UIManager.getIcon("ToolBar.verticalGrip.icon");
    }


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
        return horizontalGrip;
    }

    protected Icon getVerticalGrip() {
        return verticalGrip;
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