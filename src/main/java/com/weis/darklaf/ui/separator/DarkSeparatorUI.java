package com.weis.darklaf.ui.separator;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSeparatorUI;
import java.awt.*;

public class DarkSeparatorUI extends BasicSeparatorUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkSeparatorUI();
    }

    public void paint(final Graphics g, @NotNull final JComponent c) {
        Dimension s = c.getSize();

        g.setColor(UIManager.getDefaults().getColor("Separator.foreground"));
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            g.fillRect(0, 0, 1, s.height);
        } else {
            g.fillRect(0, 0, s.width, 1);
        }
    }

    public Dimension getPreferredSize(final JComponent c) {
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            return new Dimension(1, 0);
        } else {
            return new Dimension(0, 1);
        }
    }

    public Dimension getMinimumSize(final JComponent c) {
        return null;
    }

    public Dimension getMaximumSize(final JComponent c) {
        return null;
    }
}
