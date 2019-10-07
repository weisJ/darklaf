package com.weis.darklaf.ui.internalframe;

import com.weis.darklaf.components.border.MutableLineBorder;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkDesktopIconBorder extends MutableLineBorder implements UIResource {

    public DarkDesktopIconBorder() {
        super(1, 1, 1, 1, UIManager.getColor("DesktopIcon.borderColor"));
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        setColor(UIManager.getColor("DesktopIcon.borderColor"));
        super.paintBorder(c, g, x, y, width, height);
    }
}
