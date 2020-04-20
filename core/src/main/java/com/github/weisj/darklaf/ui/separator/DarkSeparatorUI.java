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
 *
 */
package com.github.weisj.darklaf.ui.separator;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSeparatorUI;

/**
 * @author Jannis Weis
 */
public class DarkSeparatorUI extends BasicSeparatorUI {

    protected boolean resizeLock;
    protected Color color;
    protected Dimension size;
    protected Insets insets;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSeparatorUI();
    }

    @Override
    protected void installDefaults(final JSeparator s) {
        super.installDefaults(s);
        s.setAlignmentX(Component.CENTER_ALIGNMENT);
        color = UIManager.getColor("Separator.foreground");
        size = UIManager.getDimension("Separator.size");
        insets = UIManager.getInsets("Separator.insets");
        if (insets == null) insets = new Insets(0, 0, 0, 0);
    }

    public void paint(final Graphics g, final JComponent c) {
        if (!(c instanceof JSeparator)) return;
        checkSize(c);
        g.setColor(color);
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            int height = c.getHeight() - insets.bottom - insets.top;
            int y = insets.top;
            if (height < size.height) {
                int newHeight = Math.min(size.height, c.getHeight());
                y = insets.top - (newHeight - height) / 2;
                height = newHeight;
            }
            g.fillRect(c.getWidth() / 2, y, 1, height);
        } else {
            int width = c.getWidth() - insets.left - insets.right;
            int x = insets.left;
            if (width < size.height) {
                int newWidth = Math.min(size.height, c.getWidth());
                x = insets.left - (newWidth - width) / 2;
                width = newWidth;
            }
            g.fillRect(x, c.getHeight() / 2, width, 1);
        }
    }

    private void checkSize(final JComponent c) {
        if (resizeLock) return;
        Container parent = c.getParent();
        if (parent == null) return;
        LayoutManager lm = parent.getLayout();
        if (!(lm instanceof BoxLayout || parent instanceof JToolBar)) return;
        resizeLock = true;
        Dimension dim = parent.getSize();
        Rectangle bounds = c.getBounds();
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            if (bounds.height != dim.height) c.setBounds(bounds.x, 0, bounds.width, dim.height);
        } else {
            if (bounds.width != dim.width) c.setBounds(0, bounds.y, dim.width, bounds.height);
        }
        resizeLock = false;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public Dimension getPreferredSize(final JComponent c) {
        if (c == null) return new Dimension(size);
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            return new Dimension(size.width, size.height);
        } else {
            return new Dimension(size.height, size.width);
        }
    }

    public Dimension getMinimumSize(final JComponent c) {
        return null;
    }

    public Dimension getMaximumSize(final JComponent c) {
        return null;
    }
}
