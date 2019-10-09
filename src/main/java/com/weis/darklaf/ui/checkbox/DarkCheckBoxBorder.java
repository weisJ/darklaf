package com.weis.darklaf.ui.checkbox;


import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.SystemInfo;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkCheckBoxBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isInCell(c)) {
            return new Insets(0, 0, 0, 0);
        }
        final int a = SystemInfo.isMac ? 2 : 4;
        return new InsetsUIResource(a, a, a, a);
    }

    protected boolean isInCell(final Component c) {
        return isTreeCellEditor(c) || isTableCellEditor(c) || DarkUIUtil.isInCell(c);
    }

    protected boolean isTreeCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JToggleButton.isTreeCellEditor"));
    }

    protected boolean isTableCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JToggleButton.isTreeCellEditor"));
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
