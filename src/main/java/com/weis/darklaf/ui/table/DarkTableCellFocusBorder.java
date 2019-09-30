package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.cell.DarkCellBorder;

import javax.swing.*;
import java.awt.*;

public class DarkTableCellFocusBorder extends DarkCellBorder {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
        super.paintBorder(c, g, x, y, width, height);
        g.setColor(UIManager.getColor("Table.focusBorderColor"));
        g.fillRect(0, 0, width - 1, 1);
        g.fillRect(0, 1, 1, height - 1);
        g.fillRect(1, height - 1, width - 1, 1);
        g.fillRect(width - 1, 0, 1, height - 1);
    }
}
