package com.weis.darklaf.ui.table;

import com.weis.darklaf.components.border.MutableLineBorder;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkTableBorder extends MutableLineBorder implements UIResource {

    public DarkTableBorder() {
        super(1, 1, 1, 1, null);
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
