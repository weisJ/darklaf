package com.weis.darklaf.ui.tree;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkTreeCellBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x,
                            final int y, final int width, final int height) {
        g.setColor(UIManager.getColor("Tree.editorBorderColor"));
        DarkUIUtil.drawRect(g, 0,0, width, height, 1);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(2, 5, 2, 5);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

}
