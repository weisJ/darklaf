package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.cell.DarkCellBorder;
import com.weis.darklaf.util.DarkUIUtil;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkTableCellFocusBorder extends DarkCellBorder {

    protected Color rowBorderColor;
    protected Color borderColor;

    public DarkTableCellFocusBorder() {
        rowBorderColor = UIManager.getColor("Table.focusRowBorderColor");
        borderColor = UIManager.getColor("Table.focusBorderColor");
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
        super.paintBorder(c, g, x, y, width, height);
        if (isRowFocusBorder(c)) {
            g.setColor(rowBorderColor);
            g.fillRect(0, 0, width, 1);
            g.fillRect(0, height - 1, width, 1);
            if (forcePaintLeft(c)) {
                g.fillRect(0, 0, 1, height);
            }
            if (forcePaintRight(c)) {
                g.fillRect(width - 1, 0, 1, height);
            }
        } else {
            g.setColor(borderColor);
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
