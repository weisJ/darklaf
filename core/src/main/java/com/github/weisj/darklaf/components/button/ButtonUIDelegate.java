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
package com.github.weisj.darklaf.components.button;

import java.awt.*;

import javax.accessibility.Accessible;
import javax.swing.*;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;

public class ButtonUIDelegate extends ButtonUI {

    private final ComponentUI delegate;

    public ButtonUIDelegate(final ComponentUI delegate) {
        this.delegate = delegate;
    }

    public void installUI(final JComponent c) {
        delegate.installUI(c);
    }

    public void uninstallUI(final JComponent c) {
        delegate.uninstallUI(c);
    }

    public void update(final Graphics g, final JComponent c) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
        paint(g, c);
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        delegate.paint(g, c);
    }

    public Dimension getPreferredSize(final JComponent c) {
        return delegate.getPreferredSize(c);
    }

    public Dimension getMinimumSize(final JComponent c) {
        return delegate.getMinimumSize(c);
    }

    public Dimension getMaximumSize(final JComponent c) {
        return delegate.getMaximumSize(c);
    }

    public boolean contains(final JComponent c, final int x, final int y) {
        return delegate.contains(c, x, y);
    }

    public int getBaseline(final JComponent c, final int width, final int height) {
        return delegate.getBaseline(c, width, height);
    }

    public int getAccessibleChildrenCount(final JComponent c) {
        return delegate.getAccessibleChildrenCount(c);
    }

    public Accessible getAccessibleChild(final JComponent c, final int i) {
        return delegate.getAccessibleChild(c, i);
    }
}
