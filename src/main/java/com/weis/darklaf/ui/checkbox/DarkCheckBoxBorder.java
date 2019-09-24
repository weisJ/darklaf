package com.weis.darklaf.ui.checkbox;

import com.bulenkov.iconloader.util.SystemInfo;
import com.weis.darklaf.decorators.CellRenderer;
import com.weis.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

public class DarkCheckBoxBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (DarkUIUtil.getParentOfType(CellRendererPane.class, c) != null
            || DarkUIUtil.getParentOfType(TableCellRenderer.class, c) != null
            || DarkUIUtil.getParentOfType(CellRenderer.class, c) != null
            || DarkUIUtil.getParentOfType(CellEditor.class, c) != null) {
            return new Insets(2, 5, 2, 5);
        }
        final int a = SystemInfo.isMac ? 2 : 4;
        return new InsetsUIResource(a, a, a, a);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
