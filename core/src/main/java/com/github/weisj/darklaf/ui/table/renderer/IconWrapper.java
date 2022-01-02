/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.table.renderer;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;

public class IconWrapper extends JPanel {

    private final JLabel label;
    private JComponent c;
    private int iconGap;

    protected IconWrapper() {
        setLayout(new BorderLayout());
        label = new JLabel();
        label.setIconTextGap(0);
        add(label);
    }

    protected void setIconGap(final int iconGap) {
        this.iconGap = iconGap;
    }

    protected void init(final JComponent component, final Icon icon, final ComponentOrientation orientation) {
        setComponentOrientation(orientation);
        if (c != null) {
            remove(c);
        }
        add(component);
        setComponentZOrder(component, 0);
        setComponentZOrder(label, 1);
        this.c = component;
        label.setIcon(icon);
    }

    @Override
    public void doLayout() {
        if (c == null) return;
        int w = getWidth();
        int h = getHeight();
        Border b = c.getBorder();
        Insets ins = new Insets(0, 0, 0, 0);
        Dimension labelSize = label.getPreferredSize();
        int gap = getIconCompGap();
        if (b != null) {
            ins = b.getBorderInsets(c);
        }
        if (getComponentOrientation().isLeftToRight()) {
            label.setBounds(ins.left + gap, 0, labelSize.width + 1, h);
            c.setBounds(ins.left + labelSize.width + 2 * gap - 1, 0, w - ins.left - labelSize.width - 2 * gap + 1, h);
        } else {
            c.setBounds(0, 0, w - ins.right - labelSize.width - gap - 1, h);
            label.setBounds(w - ins.right - labelSize.width - gap - 1, 0, labelSize.width + 1, h);
        }
    }

    public int getIconCompGap() {
        return iconGap;
    }
}
