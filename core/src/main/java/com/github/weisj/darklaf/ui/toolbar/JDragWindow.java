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
package com.github.weisj.darklaf.ui.toolbar;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;

public abstract class JDragWindow extends JWindow {

    protected final JToolBar toolBar;
    protected int orientation;
    protected Color borderColor;
    protected Point offset; // offset of the mouse cursor inside the DragWindow

    public JDragWindow(final Window w, final JToolBar toolBar) {
        super(w);
        this.toolBar = toolBar;
        orientation = toolBar.getOrientation();
    }

    /**
     * Sets the orientation.
     *
     * @param o the new orientation
     */
    public abstract void setOrientation(final int o);

    /**
     * Returns the offset.
     *
     * @return the offset
     */
    public Point getOffset() {
        return offset;
    }

    /**
     * Sets the offset.
     *
     * @param p the new offset
     */
    public void setOffset(final Point p) {
        this.offset = p;
    }

    public void paint(final Graphics g) {
        paintDragWindow(g);
        // Paint the children
        super.paint(g);
    }

    public Insets getInsets() {
        return new Insets(1, 1, 1, 1);
    }

    public void setBorderColor(final Color c) {
        if (this.borderColor == c) {
            return;
        }
        this.borderColor = c;
        repaint();
    }

    public Color getBorderColor() {
        return borderColor;
    }

    protected void paintDragWindow(final Graphics g) {
        int w = getWidth();
        int h = getHeight();
        g.setColor(getBackground());
        PaintUtil.fillRect(g, 0, 0, w, h);
        g.setColor(getBorderColor());
        PaintUtil.drawRect(g, 0, 0, w, h, 1);
    }
}
