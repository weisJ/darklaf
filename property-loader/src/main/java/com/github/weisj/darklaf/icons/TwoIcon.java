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

import javax.swing.*;

public class TwoIcon implements Icon {

    private int iconGap = 2;
    private Icon leftIcon;
    private Icon rightIcon;

    public TwoIcon(final Icon leftIcon, final Icon rightIcon) {
        this(leftIcon, rightIcon, 2);
    }

    public TwoIcon(final Icon leftIcon, final Icon rightIcon, final int iconGap) {
        setLeftIcon(leftIcon);
        setRightIcon(rightIcon);
        this.iconGap = iconGap;
    }

    public void setIconGap(final int iconGap) {
        this.iconGap = iconGap;
    }

    public void setLeftIcon(final Icon leftIcon) {
        this.leftIcon = leftIcon != null ? leftIcon : EmptyIcon.create(0);
    }

    public void setRightIcon(final Icon rightIcon) {
        this.rightIcon = rightIcon != null ? rightIcon : EmptyIcon.create(0);
    }

    public Icon getLeftIcon() {
        return leftIcon;
    }

    public Icon getRightIcon() {
        return rightIcon;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        int mid = getIconHeight() / 2;
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        Icon left = ltr ? leftIcon : rightIcon;
        Icon right = ltr ? rightIcon : leftIcon;
        int y1 = y + mid - left.getIconHeight() / 2;
        int y2 = y + mid - right.getIconHeight() / 2;
        leftIcon.paintIcon(c, g, x, y1);
        rightIcon.paintIcon(c, g, x + left.getIconWidth() + iconGap, y2);
    }

    @Override
    public int getIconWidth() {
        int l = leftIcon.getIconWidth();
        int r = rightIcon.getIconWidth();
        int gap = 0;
        if (l != 0 && r != 0) gap = iconGap;
        return l + r + gap;
    }

    @Override
    public int getIconHeight() {
        return Math.max(leftIcon.getIconHeight(), rightIcon.getIconHeight());
    }
}
