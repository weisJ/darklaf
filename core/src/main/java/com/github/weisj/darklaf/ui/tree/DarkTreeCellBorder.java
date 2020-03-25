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
package com.github.weisj.darklaf.ui.tree;

import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkTreeCellBorder implements Border, UIResource {

    protected final Color borderColor;
    protected Insets insets;

    public DarkTreeCellBorder() {
        borderColor = UIManager.getColor("Tree.editorBorderColor");
        insets = UIManager.getInsets("Tree.editorBorderInsets");
        if (insets == null) insets = new Insets(0, 0, 0, 0);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x,
                            final int y, final int width, final int height) {
        g.setColor(borderColor);
        DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(insets.top, insets.left, insets.bottom, insets.right);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

}
