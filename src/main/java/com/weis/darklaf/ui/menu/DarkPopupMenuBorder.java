package com.weis.darklaf.ui.menu;

import com.weis.darklaf.components.border.MutableLineBorder;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * Custom border in darcula style for Popup menu.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class DarkPopupMenuBorder extends MutableLineBorder implements UIResource {

    public DarkPopupMenuBorder() {
        super(1, 1, 1, 1, UIManager.getDefaults().getColor("PopupMenu.borderColor"));
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y, final int width, final int height) {
        setColor(UIManager.getDefaults().getColor("PopupMenu.borderColor"));
        super.paintBorder(c, g, x, y, width, height);
    }

    @NotNull
    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(1, 1, 1, 1);
    }
}
