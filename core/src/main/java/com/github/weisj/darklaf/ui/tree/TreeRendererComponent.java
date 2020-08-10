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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;

import javax.swing.*;

/**
 * @author Jannis Weis
 */
public class TreeRendererComponent extends Container {

    private static final int PAD = 5;
    private TreeRendererSupport defaultRenderer;
    private Component renderComponent;

    public TreeRendererComponent() {
        setLayout(null);
        setVisible(true);
    }

    public void setRenderer(final TreeRendererSupport renderer) {
        this.defaultRenderer = renderer;
    }

    public void setRenderComponent(final Component renderComponent) {
        removeAll();
        this.renderComponent = renderComponent;
        add(renderComponent);
    }

    @Override
    public boolean isShowing() {
        return true;
    }

    @Override
    public void doLayout() {
        if (renderComponent != null) {
            int offset = getOffset();
            int width = renderComponent.getPreferredSize().width;
            int height = getHeight();
            if (getComponentOrientation().isLeftToRight()) {
                renderComponent.setBounds(offset, 0, width, height);
            } else {
                renderComponent.setBounds(getWidth() - width - offset, 0, width, height);
            }
        }
    }

    private int getOffset() {
        Icon icon = defaultRenderer.getIcon();
        if (icon == null) return 0;
        return icon.getIconWidth() + defaultRenderer.getIconTextGap();
    }

    @Override
    public Dimension getPreferredSize() {
        if (defaultRenderer != null) {
            Dimension pSize = renderComponent.getPreferredSize();
            pSize.width += getOffset() + PAD;

            Dimension rSize = defaultRenderer.getPreferredSize();

            Icon icon = defaultRenderer.getIcon();
            if (rSize != null) {
                pSize.height = Math.max(pSize.height, rSize.height);
            }
            if (icon != null) {
                pSize.height = Math.max(pSize.height, icon.getIconHeight());
            }
            return pSize;
        }
        return new Dimension(0, 0);
    }

    @Override
    public void paint(final Graphics g) {
        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());

        int width = getWidth();

        Icon icon = defaultRenderer.getIcon();
        // Then the icon.
        if (icon != null) {
            int yLoc = calculateIconY(icon);

            if (getComponentOrientation().isLeftToRight()) {
                icon.paintIcon(this, g, 0, yLoc);
            } else {
                icon.paintIcon(this, g, width - icon.getIconWidth(), yLoc);
            }
        }
        if (renderComponent != null) renderComponent.paint(g);
    }

    /**
     * Calculate the y location for the icon.
     */
    private int calculateIconY(final Icon icon) {
        int iconHeight = icon.getIconHeight();
        int textHeight = renderComponent.getFontMetrics(renderComponent.getFont()).getHeight();
        int textY = iconHeight / 2 - textHeight / 2;
        int totalY = Math.min(0, textY);
        int totalHeight = Math.max(iconHeight, textY + textHeight) -
                          totalY;
        return getHeight() / 2 - (totalY + (totalHeight / 2));
    }

    @Override
    public void setFont(final Font f) {
        super.setFont(f);
        if (renderComponent != null) renderComponent.setFont(f);
    }

    @Override
    public void setForeground(final Color c) {
        super.setForeground(c);
        if (renderComponent != null) renderComponent.setForeground(c);
    }

    @Override
    public void setBackground(final Color c) {
        super.setBackground(c);
        if (renderComponent != null) renderComponent.setBackground(c);
    }
}
