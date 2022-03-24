/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.decorations;

import java.awt.*;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.platform.CustomTitlePane;
import com.github.weisj.darklaf.util.LogUtil;

class NativeDecorationsRootLayout implements LayoutManager2 {

    private static final Logger LOGGER = LogUtil.getLogger(NativeDecorationsRootLayout.class);

    private final AbstractNativeDecorationsRootPaneUI ui;

    NativeDecorationsRootLayout(final AbstractNativeDecorationsRootPaneUI ui) {
        this.ui = ui;
    }

    @Override
    public void addLayoutComponent(final String name, final Component comp) {}

    @Override
    public void removeLayoutComponent(final Component comp) {}

    @Override
    public void addLayoutComponent(final Component comp, final Object constraints) {}


    private boolean hasValidMenuBar(final JRootPane root) {
        return root.getJMenuBar() != null && root.getJMenuBar().getParent() == root.getLayeredPane();
    }

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        Dimension cpd, mbd, tpd;
        int cpWidth = 0;
        int cpHeight = 0;
        int mbWidth = 0;
        int mbHeight = 0;
        int tpWidth = 0;
        int tpHeight = 0;
        Insets i = parent.getInsets();

        JRootPane root = (JRootPane) parent;
        CustomTitlePane titlePane = getTitlePane(root);

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getPreferredSize();
        } else {
            cpd = root.getSize();
        }
        LOGGER.finer("[PrefSize] JRootPane content size: " + cpd);
        if (cpd != null) {
            cpWidth = cpd.width;
            cpHeight = cpd.height;
        }

        if (hasValidMenuBar(root)) {
            mbd = root.getJMenuBar().getPreferredSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }

        if (titlePane != null) {
            tpd = titlePane.getPreferredSize();
            if (tpd != null) {
                tpWidth = tpd.width;
                tpHeight = tpd.height;
            }
            LOGGER.finer("[PrefSize] JRootPane titlePane size: " + tpd);
        }

        return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth) + i.left + i.right,
                cpHeight + mbHeight + tpHeight + i.top + i.bottom);
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        Dimension cpd, mbd, tpd;
        int cpWidth = 0;
        int cpHeight = 0;
        int mbWidth = 0;
        int mbHeight = 0;
        int tpWidth = 0;
        int tpHeight = 0;
        Insets i = parent.getInsets();
        JRootPane root = (JRootPane) parent;
        CustomTitlePane titlePane = getTitlePane(root);

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getMinimumSize();
        } else {
            cpd = root.getSize();
        }
        LOGGER.finer("[MinSize] JRootPane content size: " + cpd);
        if (cpd != null) {
            cpWidth = cpd.width;
            cpHeight = cpd.height;
        }

        if (hasValidMenuBar(root)) {
            mbd = root.getJMenuBar().getMinimumSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }

        if (titlePane != null) {
            tpd = titlePane.getMinimumSize();
            if (tpd != null) {
                tpWidth = tpd.width;
                tpHeight = tpd.height;
            }
            LOGGER.finer("[MinSize] JRootPane titlePane size: " + tpd);
        }

        return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth) + i.left + i.right,
                cpHeight + mbHeight + tpHeight + i.top + i.bottom);
    }

    protected CustomTitlePane getTitlePane(final JRootPane root) {
        return ui.titlePane();
    }

    @Override
    public void layoutContainer(final Container parent) {
        JRootPane root = (JRootPane) parent;
        Rectangle b = root.getBounds();

        Insets i = root.getInsets();

        b.setLocation(0, 0);
        b.x += i.left;
        b.y += i.top;
        b.width -= i.left + i.right;
        b.height -= i.top + i.bottom;

        if (root.getLayeredPane() != null) {
            root.getLayeredPane().setBounds(b.x, b.y, b.width, b.height);
        }
        if (root.getGlassPane() != null) {
            root.getGlassPane().setBounds(b.x, b.y, b.width, b.height);
        }

        ui.decorationsManager().adjustContentArea(root, b);

        layoutContent(root, b.x, b.y, b.width, b.height);
    }

    public void layoutContent(final JRootPane root, final int x, final int y, final int w, final int h) {
        int nextY = y;
        JComponent titlePane = getTitlePane(root);
        if (titlePane != null) {
            Dimension tpd = titlePane.getPreferredSize();
            if (tpd != null) {
                int tpHeight = tpd.height;
                titlePane.setBounds(x, y, w, tpHeight);
                nextY += tpHeight;
            }
        }
        if (hasValidMenuBar(root)) {
            Dimension mbd = root.getJMenuBar().getPreferredSize();
            root.getJMenuBar().setBounds(x, nextY, w, mbd.height);
            nextY += mbd.height;
        }
        if (root.getContentPane() != null) {
            root.getContentPane().setBounds(x, nextY, w, h < nextY ? 0 : h - nextY);
        }
    }

    @Override
    public Dimension maximumLayoutSize(final Container target) {
        Dimension cpd, mbd, tpd;
        int cpWidth = Integer.MAX_VALUE;
        int cpHeight = Integer.MAX_VALUE;
        int mbWidth = Integer.MAX_VALUE;
        int mbHeight = Integer.MAX_VALUE;
        int tpWidth = Integer.MAX_VALUE;
        int tpHeight = Integer.MAX_VALUE;
        Insets i = target.getInsets();
        JRootPane root = (JRootPane) target;

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getMaximumSize();
            if (cpd != null) {
                cpWidth = cpd.width;
                cpHeight = cpd.height;
            }
        }

        if (hasValidMenuBar(root)) {
            mbd = root.getJMenuBar().getMaximumSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }

        if ((root.getWindowDecorationStyle() != JRootPane.NONE)) {
            JComponent titlePane = ui.titlePane();
            if (titlePane != null) {
                tpd = titlePane.getMaximumSize();
                if (tpd != null) {
                    tpWidth = tpd.width;
                    tpHeight = tpd.height;
                }
            }
        }

        int maxHeight = Math.max(Math.max(cpHeight, mbHeight), tpHeight);
        if (maxHeight != Integer.MAX_VALUE) {
            maxHeight = cpHeight + mbHeight + tpHeight + i.top + i.bottom;
        }

        int maxWidth = Math.max(Math.max(cpWidth, mbWidth), tpWidth);

        if (maxWidth != Integer.MAX_VALUE) {
            maxWidth += i.left + i.right;
        }

        return new Dimension(maxWidth, maxHeight);
    }

    @Override
    public float getLayoutAlignmentX(final Container target) {
        return 0.0f;
    }

    @Override
    public float getLayoutAlignmentY(final Container target) {
        return 0.0f;
    }

    @Override
    public void invalidateLayout(final Container target) {}
}
