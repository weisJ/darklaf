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
package com.weis.darklaf.ui.tabbedpane;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class CroppedEdge extends JPanel implements UIResource {
    private DarkTabbedPaneUIBridge ui;
    protected Shape shape;
    protected int tabIndex;
    protected int cropline;
    protected int cropx, cropy;

    public CroppedEdge(final DarkTabbedPaneUIBridge ui) {
        this.ui = ui;
        setOpaque(false);
    }

    public void setParams(final int tabIndex, final int cropline, final int cropx, final int cropy) {
        this.tabIndex = tabIndex;
        this.cropline = cropline;
        this.cropx = cropx;
        this.cropy = cropy;
        Rectangle tabRect = ui.rects[tabIndex];
        setBounds(tabRect);
        shape = DarkTabbedPaneUIBridge.createCroppedTabShape(ui.tabPane.getTabPlacement(), tabRect, cropline);
        if (getParent() == null && ui.tabContainer != null) {
            ui.tabContainer.add(this, 0);
        }
    }

    public void resetParams() {
        shape = null;
        if (getParent() == ui.tabContainer && ui.tabContainer != null) {
            ui.tabContainer.remove(this);
        }
    }

    public int getTabIndex() {
        return tabIndex;
    }

    public int getCropline() {
        return cropline;
    }

    public int getCroppedSideWidth() {
        return 3;
    }

    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (isParamsSet() && g instanceof Graphics2D) {
            Graphics2D g2 = (Graphics2D) g;
            g2.clipRect(0, 0, getWidth(), getHeight());
            g2.setColor(getBgColor());
            g2.translate(cropx, cropy);
            g2.fill(shape);
            ui.paintCroppedTabEdge(g);
            g2.translate(-cropx, -cropy);
        }
    }

    public boolean isParamsSet() {
        return shape != null;
    }

    protected Color getBgColor() {
        Component parent = ui.tabPane.getParent();
        if (parent != null) {
            Color bg = parent.getBackground();
            if (bg != null) {
                return bg;
            }
        }
        return UIManager.getColor("control");
    }
}
