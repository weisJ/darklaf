package com.weis.darklaf.ui.menu;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkMenuBarBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y, int w, int h) {
        g.setColor(UIManager.getColor("MenuBar.darcula.borderColor"));
        g.fillRect(x, y + h - 1, w, 1);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(0, 0, 1, 0);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }
}
