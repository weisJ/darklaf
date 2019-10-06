package com.weis.darklaf.ui.table;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkTableHeaderCorner extends JComponent implements UIResource {

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        g.setColor(getHeaderBackground());
        g.fillRect(0, 0, getWidth(), getHeight());

        g.setColor(getBorderColor());
        g.fillRect(0, getHeight() - 1, getWidth(), 1);
    }

    protected Color getHeaderBackground() {
        return UIManager.getColor("TableHeader.background");
    }

    protected Color getBorderColor() {
        return UIManager.getColor("TableHeader.borderColor");
    }
}
