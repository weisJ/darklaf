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
package com.github.weisj.darklaf.ui.internalframe;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public class InternalFrameMenuBar extends JComponent {

    protected Color borderColor;
    protected JMenuBar menuBar;

    public InternalFrameMenuBar() {
        setBackground(UIManager.getColor("InternalFrameMenuBar.background"));
        borderColor = UIManager.getColor("InternalFrameMenuBar.borderColor");
    }

    public void setActiveJMenuBar(final JMenuBar menuBar) {
        if (menuBar != null) {
            add(menuBar);
        } else if (this.menuBar != null) {
            remove(this.menuBar);
        }
        this.menuBar = menuBar;
        getParent().doLayout();
        doLayout();
        repaint();
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (menuBar != null) {
            g.setColor(getBackground());
            g.fillRect(0, 0, getWidth(), getHeight());
            g.setColor(borderColor);
            g.fillRect(0, getHeight() - 1, getWidth(), 1);
        }
    }

    @Override
    public Dimension getPreferredSize() {
        return DarkUIUtil.getPreferredSize(menuBar);
    }

    @Override
    public void doLayout() {
        if (menuBar != null) {
            menuBar.setBounds(0, 0, DarkUIUtil.getPreferredSize(menuBar).width, getHeight());
            menuBar.doLayout();
        }
    }
}
