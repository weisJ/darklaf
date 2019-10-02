package com.weis.darklaf.ui.radiobutton;


import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.SystemInfo;

import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkRadioButtonBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y,
                            final int width, final int height) {
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (DarkUIUtil.isInTableCell(c)) {
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
