/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.ui.menu;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.platform.CustomTitlePane;

/** @author Jannis Weis */
public class DarkMenuBarBorder implements Border, UIResource, CustomTitlePane.BorderCollapseHint {

    protected final Color borderColor;
    private final Insets margins;

    public DarkMenuBarBorder() {
        borderColor = UIManager.getColor("MenuBar.borderColor");
        Insets m = UIManager.getInsets("MenuBar.border.margins");
        if (m == null) m = new Insets(0, 0, 0, 0);
        margins = m;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int w, final int h) {
        g.setColor(borderColor);
        g.fillRect(x, y + h - 1, w, getBottomCollapse());
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(margins.top, margins.left, margins.bottom + getBottomCollapse(), margins.right);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    @Override
    public int getBottomCollapse() {
        return 1;
    }
}
