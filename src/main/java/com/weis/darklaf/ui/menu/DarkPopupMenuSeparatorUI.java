package com.weis.darklaf.ui.menu;

import com.weis.darklaf.ui.separator.DarkSeparatorUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

public class DarkPopupMenuSeparatorUI extends DarkSeparatorUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkPopupMenuSeparatorUI();
    }


    @Override
    public void paint(@NotNull final Graphics g, @NotNull final JComponent c) {
        Dimension s = c.getSize();
        g.setColor(UIManager.getDefaults().getColor("PopupMenu.borderColor"));
        g.fillRect(0, 0, s.width, 1);
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        return new Dimension(0, 1);
    }
}
