package com.weis.darklaf.ui.spinner;

import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.ui.text.DarkTextBorder;
import com.weis.darklaf.ui.text.DarkTextFieldUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class DarkSpinnerBorder implements Border, UIResource {

    public static int BORDER_SIZE = DarkTextBorder.BORDER_SIZE;
    public static int ARC_SIZE = DarkTextFieldUI.SEARCH_ARC_SIZE;

    @Override
    public void paintBorder(@NotNull final Component c, final Graphics g2,
                            final int x, final int y, final int width, final int height) {
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        int size = BORDER_SIZE;
        int arc = ARC_SIZE;
        g.setColor(getBorderColor(c));
        DarkUIUtil.paintLineBorder(g, size, size, width - 2 * size, height - 2 * size, arc, true);

        if (c instanceof JSpinner) {
            JSpinner spinner = (JSpinner)c;
            JComponent editor = spinner.getEditor();
            if (editor != null) {
                int off = spinner.getComponentOrientation().isLeftToRight()
                          ? editor.getBounds().x + editor.getWidth()
                          : editor.getBounds().x - 1 - BORDER_SIZE;
                g.setColor(getBorderColor(spinner));
                g.fillRect(off, size, 1, height - 2 * size);
            }
        }

        if (DarkUIUtil.hasFocus(c)) {
            g.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            DarkUIUtil.paintFocusBorder(g, width, height, arc, true);
        }
        g.translate(-x, -y);
        config.restore();
    }

    private Color getBorderColor(@NotNull final Component c) {
        return c.isEnabled() ? UIManager.getColor("Spinner.activeBorderColor")
                             : UIManager.getColor("Spinner.inactiveBorderColor");
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(7, 7, 7, 7);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }
}
