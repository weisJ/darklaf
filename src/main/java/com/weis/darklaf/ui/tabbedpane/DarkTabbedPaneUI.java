package com.weis.darklaf.ui.tabbedpane;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

public class DarkTabbedPaneUI extends DarkTabbedPaneUIBridge {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabbedPaneUI();
    }

    @Override
    protected void paintTabBorder(final Graphics g, final int tabPlacement, final int tabIndex,
                                  final int x, final int y, final int w, final int h,
                                  final boolean isSelected) {
        super.paintTabBorder(g, tabPlacement, tabIndex, x, y, w, h, isSelected);
    }

    @Override
    protected void paintFocusIndicator(final Graphics g, final int tabPlacement, final Rectangle[] rects, final int tabIndex,
                                       final Rectangle iconRect, final Rectangle textRect, final boolean isSelected) {
        if (isSelected) {
            g.setColor(UIManager.getColor("DnDTabbedPane.selectionAccentUnfocused"));
            if (DarkUIUtil.hasFocus(tabPane)) {
                g.setColor(UIManager.getColor("DnDTabbedPane.selectionAccent"));
            }
            var r = rects[tabIndex];
            g.fillRect(r.x, r.y + r.height - 4, r.width, 4);
        }
    }
}
