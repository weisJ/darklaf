package com.weis.darklaf.ui.toolbar;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class DropPreviewPanel extends JComponent {

    private JToolBar toolBar;

    public void setToolBar(final JToolBar toolBar) {
        this.toolBar = toolBar;
    }

    @Override
    public void paint(@NotNull final Graphics g) {
        g.setColor(getBackgroundColor());
        g.fillRect(0, 0, getWidth(), getHeight());
    }

    protected Color getBackgroundColor() {
        var useToolbar = Boolean.TRUE.equals(toolBar.getClientProperty("JToolBar.drag.useToolbarBackground"));
        if (!useToolbar) {
            var c = UIManager.getColor("ToolBar.dropColor");
            if (c == null) {
                return toolBar.getBackground();
            }
        }
        return toolBar.getBackground();
    }

    @Override
    public Dimension getPreferredSize() {
        if (toolBar != null) { return toolBar.getPreferredSize(); }
        return super.getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        if (toolBar != null) { return toolBar.getMaximumSize(); }
        return super.getMinimumSize();
    }

    @Override
    public Dimension getMinimumSize() {
        if (toolBar != null) { return toolBar.getMinimumSize(); }
        return super.getMinimumSize();
    }
}
