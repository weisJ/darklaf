package com.weis.darklaf.ui.tree;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class TreeRendererComponent extends Container {

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
        remove(renderComponent);
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
        var icon = defaultRenderer.getIcon();
        if (icon == null) return 0;
        return icon.getIconWidth() + defaultRenderer.getIconTextGap();
    }

    @Override
    public Dimension getPreferredSize() {
        if (defaultRenderer != null) {
            Dimension pSize = renderComponent.getPreferredSize();
            pSize.width += getOffset() + 5;

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
        int width = getWidth();

        var icon = defaultRenderer.getIcon();
        // Then the icon.
        if (icon != null) {
            int yLoc = calculateIconY(icon);

            if (getComponentOrientation().isLeftToRight()) {
                icon.paintIcon(this, g, 0, yLoc);
            } else {
                icon.paintIcon(this, g, width - icon.getIconWidth(), yLoc);
            }
        }
        super.paint(g);
    }

    /**
     * Calculate the y location for the icon.
     */
    private int calculateIconY(@NotNull final Icon icon) {
        int iconHeight = icon.getIconHeight();
        int textHeight = renderComponent.getFontMetrics(renderComponent.getFont()).getHeight();
        int textY = iconHeight / 2 - textHeight / 2;
        int totalY = Math.min(0, textY);
        int totalHeight = Math.max(iconHeight, textY + textHeight) -
                totalY;
        return getHeight() / 2 - (totalY + (totalHeight / 2));
    }
}
