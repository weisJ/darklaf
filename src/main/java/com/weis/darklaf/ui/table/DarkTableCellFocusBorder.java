package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.cell.DarkCellBorder;
import com.weis.darklaf.util.DarkUIUtil;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkTableCellFocusBorder extends DarkCellBorder {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
        super.paintBorder(c, g, x, y, width, height);
        g.setColor(UIManager.getColor("Table.focusBorderColor"));
        DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
    }
}
