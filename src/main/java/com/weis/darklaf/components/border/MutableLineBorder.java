package com.weis.darklaf.components.border;

import org.jetbrains.annotations.NotNull;

import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Line Border that can change side thickness.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class MutableLineBorder extends EmptyBorder {

    private Color color;

    public MutableLineBorder(
            final int top, final int left, final int bottom, final int right, final Color color) {
        super(top, left, bottom, right);
        this.color = color;
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(getColor());
        var insets = getBorderInsets();
        g.fillRect(x, y, width - insets.right, insets.top);
        g.fillRect(x, y + insets.top, insets.left, height - insets.top);
        g.fillRect(x + insets.left, y + height - insets.bottom, width - insets.left, insets.bottom);
        g.fillRect(x + width - insets.right, y, insets.right, height - insets.bottom);
    }

    public void setLeft(final int left) {
        this.left = left;
    }

    public void setRight(final int right) {
        this.right = right;
    }

    public void setTop(final int top) {
        this.top = top;
    }

    public void setBottom(final int bottom) {
        this.bottom = bottom;
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    protected Color getColor() {
        return color;
    }

    public void setColor(final Color color) {
        this.color = color;
    }
}
