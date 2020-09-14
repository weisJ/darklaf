/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.toolbar;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.plaf.UIResource;

/** @author Jannis Weis */
public class DarkToolBarBorder extends AbstractBorder implements UIResource, SwingConstants {

    protected final Icon horizontalGrip;
    protected final Icon verticalGrip;
    protected final Color borderColor;
    protected final Insets gripInsets;

    public DarkToolBarBorder() {
        horizontalGrip = UIManager.getIcon("ToolBar.horizontalGrip.icon");
        verticalGrip = UIManager.getIcon("ToolBar.verticalGrip.icon");
        borderColor = UIManager.getColor("ToolBar.borderColor");
        gripInsets = UIManager.getInsets("ToolBar.gripInsets");
    }

    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int w, final int h) {
        g.translate(x, y);
        if (isFloatable(c)) {
            if (((JToolBar) c).getOrientation() == HORIZONTAL) {
                Icon icon = getHorizontalGrip();
                int yIcon = h / 2 - icon.getIconHeight() / 2;
                if (c.getComponentOrientation().isLeftToRight()) {
                    icon.paintIcon(c, g, gripInsets.left, yIcon);
                } else {
                    icon.paintIcon(c, g, w - icon.getIconWidth() - gripInsets.right, yIcon);
                }
            } else {
                Icon icon = getVerticalGrip();
                int xIcon = w / 2 - icon.getIconWidth() / 2;
                icon.paintIcon(c, g, xIcon, gripInsets.top);
            }
        }
        if (isDocked(c)) {
            String constraints = getDockedConstrains(c);
            JToolBar toolBar = (JToolBar) c;
            g.setColor(borderColor);
            if (toolBar.getOrientation() == JToolBar.HORIZONTAL) {
                if (BorderLayout.NORTH.equals(constraints) || BorderLayout.PAGE_START.equals(constraints)) {
                    g.fillRect(0, toolBar.getHeight() - 1, toolBar.getWidth(), 1);
                } else if (BorderLayout.SOUTH.equals(constraints) || BorderLayout.PAGE_END.equals(constraints)) {
                    g.fillRect(0, 0, toolBar.getWidth(), 1);
                }
            } else {
                if (BorderLayout.WEST.equals(constraints) || BorderLayout.LINE_START.equals(constraints)) {
                    g.fillRect(toolBar.getWidth() - 1, 0, 1, toolBar.getHeight());
                } else if (BorderLayout.EAST.equals(constraints) || BorderLayout.LINE_END.equals(constraints)) {
                    g.fillRect(0, 0, 1, toolBar.getHeight());
                }
            }
        }
        g.translate(-x, -y);
    }

    private boolean isDocked(final Component c) {
        if (c instanceof JToolBar && ((JToolBar) c).getUI() instanceof DarkToolBarUI) {
            return !((DarkToolBarUI) ((JToolBar) c).getUI()).isFloating();
        }
        return false;
    }

    private String getDockedConstrains(final Component c) {
        if (c instanceof JComponent) {
            Component parent = c.getParent();
            if (parent instanceof JComponent && ((JComponent) parent).getLayout() instanceof BorderLayout) {
                BorderLayout layout = (BorderLayout) ((JComponent) parent).getLayout();
                Object constraints = layout.getConstraints(c);
                if (constraints != null) return constraints.toString();
            }
        }
        return "";
    }

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
                Icon icon = getHorizontalGrip();
                if (c.getComponentOrientation().isLeftToRight()) {
                    newInsets.left = icon.getIconWidth() + gripInsets.left + gripInsets.right;
                } else {
                    newInsets.right = icon.getIconWidth() + gripInsets.left + gripInsets.right;
                }
            } else {
                newInsets.top = getVerticalGrip().getIconHeight() + gripInsets.top + gripInsets.bottom;
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
        if (c instanceof JToolBar && isDocked(c)) {
            String constraints = getDockedConstrains(c);
            if (((JToolBar) c).getOrientation() == JToolBar.HORIZONTAL) {
                if (BorderLayout.NORTH.equals(constraints)) {
                    newInsets.bottom++;
                } else if (BorderLayout.SOUTH.equals(constraints)) {
                    newInsets.top++;
                }
            } else {
                if (BorderLayout.WEST.equals(constraints)) {
                    newInsets.right++;
                } else if (BorderLayout.EAST.equals(constraints)) {
                    newInsets.left++;
                }
            }
        }
        return newInsets;
    }
}
