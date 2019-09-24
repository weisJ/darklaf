package com.weis.darklaf.ui.statusbar;

import org.jdesktop.swingx.JXStatusBar;
import org.jdesktop.swingx.plaf.basic.BasicStatusBarUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

public class DarkStatusBarUI extends BasicStatusBarUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkStatusBarUI();
    }

    @Override
    protected void paintBackground(final Graphics2D g, @NotNull final JXStatusBar bar) {
        if (bar.isOpaque()) {
            g.setColor(UIManager.getColor("StatusBar.background"));
            g.fillRect(0, 0, bar.getWidth(), bar.getHeight());
        }
        g.setColor(UIManager.getColor("StatusBar.topColor"));
        g.fillRect(0, 0, bar.getWidth(), 1);
    }
}
