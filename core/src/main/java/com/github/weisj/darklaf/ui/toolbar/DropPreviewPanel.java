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
package com.github.weisj.darklaf.ui.toolbar;

import java.awt.*;

import javax.swing.*;

/**
 * @author Jannis Weis
 */
public class DropPreviewPanel extends JComponent {

    private JToolBar toolBar;
    private final Color dropBackground;

    public DropPreviewPanel() {
        dropBackground = UIManager.getColor("ToolBar.dropColor");
    }

    public void setToolBar(final JToolBar toolBar) {
        this.toolBar = toolBar;
    }

    @Override
    public void paint(final Graphics g) {
        g.setColor(getBackgroundColor());
        g.fillRect(0, 0, getWidth(), getHeight());
    }

    protected Color getBackgroundColor() {
        boolean useToolbar = Boolean.TRUE.equals(toolBar.getClientProperty(DarkToolBarUI.KEY_USE_TOOL_BAR_BACKGROUND));
        if (!useToolbar) {
            Color c = dropBackground;
            if (c == null) {
                return toolBar.getBackground();
            }
            return c;
        }
        return toolBar.getBackground();
    }

    @Override
    public Dimension getPreferredSize() {
        if (toolBar != null) {
            return toolBar.getPreferredSize();
        }
        return super.getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        if (toolBar != null) {
            return toolBar.getMaximumSize();
        }
        return super.getMinimumSize();
    }

    @Override
    public Dimension getMinimumSize() {
        if (toolBar != null) {
            return toolBar.getMinimumSize();
        }
        return super.getMinimumSize();
    }
}
