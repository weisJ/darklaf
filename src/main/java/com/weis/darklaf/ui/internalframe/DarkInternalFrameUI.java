package com.weis.darklaf.ui.internalframe;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicInternalFrameUI;
import java.awt.*;

public class DarkInternalFrameUI extends BasicInternalFrameUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent b) {
        return new DarkInternalFrameUI((JInternalFrame) b);
    }


    public DarkInternalFrameUI(final JInternalFrame b) {
        super(b);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        frame.setOpaque(false);
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        super.paint(g, c);
    }

    @Override
    protected JComponent createNorthPane(final JInternalFrame w) {
        this.titlePane = new DarkInternalFrameTitlePane(w);
        this.titlePane.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 0));
        return this.titlePane;
    }
}
