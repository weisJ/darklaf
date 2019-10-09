package com.weis.darklaf.ui.rootpane;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class TitlePaneIcon implements Icon {

    private final Icon activeIcon;
    private final Icon inactiveIcon;
    private boolean active = true;

    @Contract(pure = true)
    public TitlePaneIcon(final Icon active, final Icon inactive) {
        this.activeIcon = active;
        this.inactiveIcon = inactive;
    }

    public void setActive(final boolean active) {
        this.active = active;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        currentIcon().paintIcon(c, g, x, y);
    }

    @Contract(pure = true)
    private Icon currentIcon() {
        return active ? activeIcon : inactiveIcon;
    }

    @Override
    public int getIconWidth() {
        return currentIcon().getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return currentIcon().getIconHeight();
    }
}
