package com.weis.darklaf.ui.internalframe;

import com.weis.darklaf.components.border.DropShadowBorder;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkInternalFrameBorder extends DropShadowBorder implements UIResource {

    public DarkInternalFrameBorder() {
        super(Color.BLACK, 9, .1f, 9,
              true, true, true, true);
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics graphics, final int x, final int y,
                            final int width, final int height) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isMaximum()) {
            return;
        }
        updateSize(c);
        setShadowColor(UIManager.getColor("InternalFrame.borderShadowColor"));
        super.paintBorder(c, graphics, x, y, width, height);
    }

    protected void updateSize(final Component c) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isSelected()) {
            setShadowOpacity(0.2f);
        } else {
            setShadowOpacity(0.1f);
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isMaximum()) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        updateSize(c);
        return super.getBorderInsets(c);
    }
}
