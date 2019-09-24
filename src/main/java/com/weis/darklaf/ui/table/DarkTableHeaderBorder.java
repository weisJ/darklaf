package com.weis.darklaf.ui.table;

import com.weis.darklaf.components.border.MutableLineBorder;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkTableHeaderBorder extends MutableLineBorder implements UIResource {

    public DarkTableHeaderBorder() {
        super(0, 0, 1, 0, null);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
        setColor(getBorderColor());
        super.paintBorder(c, g, x, y, width, height);
    }

    protected Color getBorderColor() {
        return UIManager.getColor("TableHeader.borderColor");
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }
}
