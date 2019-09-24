package com.weis.darklaf.ui.splitpane;

import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkSplitPaneDividerBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new Insets(0, 0, 0, 0);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
