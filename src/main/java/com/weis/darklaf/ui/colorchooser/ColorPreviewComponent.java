package com.weis.darklaf.ui.colorchooser;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
final class ColorPreviewComponent extends JComponent {
    private Color myColor;

    ColorPreviewComponent() {
        setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 2));
    }

    public void setColor(final Color c) {
        myColor = c;
        repaint();
    }

    @Override
    protected void paintComponent(@NotNull final Graphics g) {
        final Insets i = getInsets();
        final Rectangle r = getBounds();

        final int width = r.width - i.left - i.right;
        final int height = r.height - i.top - i.bottom;

        g.setColor(Color.WHITE);
        g.fillRect(i.left, i.top, width, height);

        g.setColor(myColor);
        g.fillRect(i.left + 1, i.top + 1, width - 2, height - 2);

        g.setColor(UIManager.getColor("ColorChooser.previewBorderColor"));
        g.fillRect(i.left, i.top, width, 1);
        g.fillRect(i.left, i.top, 1, height);
        g.fillRect(i.left + width - 1, i.top, 1, height);
        g.fillRect(i.left, i.top + height - 1, width, 1);
    }

    @NotNull
    @Contract(value = " -> new", pure = true)
    @Override
    public Dimension getPreferredSize() {
        return new Dimension(100, 32);
    }
}
