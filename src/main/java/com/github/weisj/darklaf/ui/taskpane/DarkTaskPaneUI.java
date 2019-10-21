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
package com.github.weisj.darklaf.ui.taskpane;

import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jdesktop.swingx.JXCollapsiblePane;
import org.jdesktop.swingx.JXHyperlink;
import org.jdesktop.swingx.JXTaskPane;
import org.jdesktop.swingx.plaf.metal.MetalTaskPaneUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkTaskPaneUI extends MetalTaskPaneUI {

    protected boolean isCollapsed;
    protected Color borderColor;
    protected Icon collapsedIcon;
    protected Icon openIcon;
    protected int arc;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTaskPaneUI();
    }

    @Override
    protected void installDefaults() {
        borderColor = UIManager.getColor("TaskPane.borderColor");
        arc = UIManager.getInt("TaskPane.arc");
        collapsedIcon = UIManager.getIcon("TaskPane.collapsed.icon");
        openIcon = UIManager.getIcon("TaskPane.open.icon");
        super.installDefaults();
    }

    @Override
    protected Border createPaneBorder() {
        return new DarkPaneBorder();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        group.addPropertyChangeListener(JXCollapsiblePane.ANIMATION_STATE_KEY, e -> {
            isCollapsed = "collapsed".equals(e.getNewValue());
        });
    }

    @Override
    protected Border createContentPaneBorder() {
        return new CompoundBorder(new DarkContentPaneBorder(borderColor, arc),
                                  BorderFactory.createEmptyBorder(10, 10, 10, 10));
    }

    @Override
    protected void configure(final JXHyperlink link) {
        super.configure(link);
        link.setFocusPainted(false);
    }

    @Override
    protected int getRoundHeight() {
        return arc;
    }

    protected boolean isCollapsed() {
        if (!group.isAnimated()) return group.isCollapsed();
        return isCollapsed;
    }

    protected static class DarkContentPaneBorder implements Border, UIResource {
        protected Color color;
        protected int arc;

        @Contract(pure = true)
        public DarkContentPaneBorder(final Color color, final int arc) {
            this.arc = arc;
            this.color = color;
        }

        public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                                final int width, final int height) {
            var clip = g.getClip().getBounds();
            int h = height + arc;
            g.setClip(clip.x, clip.y, width, h / 2 + 1);
            g.setColor(color);
            DarkUIUtil.drawRect(g, x, y - 1, width, h, 1);
            if (c.isOpaque()) {
                g.setColor(c.getBackground());
                g.setClip(clip.x, clip.x + h - arc, width, arc);
                DarkUIUtil.fillRoundRect((Graphics2D) g, x + 0.5f, y + 0.5f, width - 1, h - 1, arc);
            }
            g.setClip(clip.x, clip.y + h / 2 - 1, width, h / 2 + 2);
            g.setColor(color);
            DarkUIUtil.paintLineBorder((Graphics2D) g, x, y, width, h, arc, false);
            g.setClip(clip);
        }

        public Insets getBorderInsets(final Component c) {
            return new Insets(0, 1, 1, 1);
        }

        public boolean isBorderOpaque() {
            return true;
        }
    }

    protected class DarkPaneBorder extends PaneBorder {

        @Override
        protected void paintTitleBackground(@NotNull final JXTaskPane group, final Graphics g2) {
            var g = (Graphics2D) g2;
            int w = group.getWidth();
            int h = getTitleHeight(group);
            if (group.isSpecial()) {
                g.setColor(specialTitleBackground);
            } else {
                g.setColor(titleBackgroundGradientStart);
            }
            if (isCollapsed()) {
                DarkUIUtil.fillRoundRect(g, 0.5f, 0.5f, w - 1, h - 1, getRoundHeight());
                g.setColor(borderColor);
                DarkUIUtil.paintLineBorder(g, 0, 0, w, h, getRoundHeight(), false);
            } else {
                var clip = g.getClip().getBounds();
                g.setClip(0, 0, w, h / 2 + 1);

                DarkUIUtil.fillRoundRect(g, 0.5f, 0.5f, w - 1, h - 1, getRoundHeight());
                g.setClip(0, h / 2 - 1, w, h / 2 + 2);
                g.fillRect(0, 0, w, h);
                g.setColor(borderColor);
                g.setClip(0, 0, w, h / 2 + 1);
                DarkUIUtil.paintLineBorder(g, 0, 0, w, h, getRoundHeight(), false);
                g.setClip(0, h / 2 - 1, w, h / 2 + 2);
                DarkUIUtil.drawRect(g, 0, 0, w, h, 1);
                g.setClip(clip);
            }
        }

        @Override
        protected void configureLabel(final JXTaskPane group) {
            super.configureLabel(group);
            label.setFont(label.getFont().deriveFont(Font.PLAIN));
        }

        @Override
        protected void paintExpandedControls(final JXTaskPane group, @NotNull final Graphics g, final int x,
                                             final int y, final int width, final int height) {
            ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g.setColor(getPaintColor(group));
            if (group.isCollapsed()) {
                collapsedIcon.paintIcon(group, g, x, y);
            } else {
                openIcon.paintIcon(group, g, x, y);
            }
            ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
        }

        @Override
        protected boolean isMouseOverBorder() {
            return true;
        }
    }
}
