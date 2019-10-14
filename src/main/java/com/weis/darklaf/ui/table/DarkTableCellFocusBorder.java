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
        if (isRowFocusBorder(c)) {
            g.setColor(UIManager.getColor("Table.focusRowBorderColor"));
            ((Graphics2D) g).scale(0.5, 0.5);
            g.drawRect(0, 0, 2 * width, 1);
            g.drawRect(0, 2 * height - 2, 2 * width, 1);
            if (forcePaintLeft(c)) {
                g.drawRect(0, 0, 1, 2 * height);
            }
            if (forcePaintRight(c)) {
                g.drawRect(2 * width - 2, 0, 1, 2 * height);
            }
        } else {
            g.setColor(UIManager.getColor("Table.focusBorderColor"));
            DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
        }
    }

    protected static boolean isRowFocusBorder(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTable.rowFocusBorder"));
    }

    protected static boolean forcePaintLeft(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTable.forcePaintLeft"));
    }

    protected static boolean forcePaintRight(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTable.forcePaintRight"));
    }
}
