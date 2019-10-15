package com.weis.darklaf.ui.internalframe;

import com.weis.darklaf.components.border.MutableLineBorder;
import com.weis.darklaf.defaults.DarkColors;
import org.jetbrains.annotations.NotNull;

import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkDesktopIconBorder extends MutableLineBorder implements UIResource {

    public DarkDesktopIconBorder() {
        super(1, 1, 1, 1, DarkColors.get().getDesktopIconBorderColor());
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        setColor(DarkColors.get().getDesktopIconBorderColor());
        super.paintBorder(c, g, x, y, width, height);
    }
}
