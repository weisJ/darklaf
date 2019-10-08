package com.weis.darklaf.ui.text;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * Text Border that supports error highlighting.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkTextBorder implements Border, UIResource {

    public final static int BORDER_SIZE = 3;
    public final static int PADDING = 4;

    public void paintBorder(final Component c, final Graphics g2, final int x, final int y,
                            final int width, final int height) {
        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        int arcSize = DarkTextFieldUI.getArcSize(c);
        if (!DarkTextFieldUI.isSearchField(c)) {
            if (DarkTextFieldUI.hasError(c)) {
                DarkUIUtil.paintOutlineBorder(g, width, height, arcSize, true,
                                              c.hasFocus(), DarkUIUtil.Outline.error);
            } else if (c.hasFocus()) {
                DarkUIUtil.paintFocusBorder(g, width, height, arcSize, true);
            }
            g.setColor(DarkTextFieldUI.getBorderColor(c));
            if (DarkTextFieldUI.chooseAlternativeArc(c)) {
                DarkUIUtil.paintLineBorder(g, BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                                           height - 2 * BORDER_SIZE, arcSize, true);
            } else {
                g.drawRect(BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE, height - 2 * BORDER_SIZE);
            }
        } else if (DarkTextFieldUI.isSearchField(c)) {
            g.setColor(DarkTextFieldUI.getBorderColor(c));
            if (((JComponent) c).getClientProperty("JTextField.Search.noBorderRing") != Boolean.TRUE) {
                if (c.hasFocus()) {
                    DarkUIUtil.paintOutlineBorder(g, width, height, arcSize, true,
                                                  true, DarkUIUtil.Outline.focus);
                }
                g.setColor(DarkTextFieldUI.getBorderColor(c));
                DarkUIUtil.paintLineBorder(g, BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                                           height - 2 * BORDER_SIZE, arcSize, true);
            }
        }
        g.translate(-x, -y);
        config.restore();
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        Insets insets = new Insets(BORDER_SIZE + PADDING, BORDER_SIZE + PADDING,
                                   BORDER_SIZE + PADDING, BORDER_SIZE + PADDING);
        if (DarkTextFieldUI.isSearchField(c)) {
            int searchWidth = DarkTextFieldUI.getSearchIcon(c).getIconWidth();
            int clearWidth = DarkTextFieldUI.getClearIcon().getIconWidth();
            insets.left += PADDING + searchWidth;
            insets.right += PADDING + clearWidth;
        } else if (DarkPasswordFieldUI.hasShowIcon(c)) {
            int eyeWidth = DarkPasswordFieldUI.getShowIcon().getIconWidth();
            insets.right += PADDING + eyeWidth;
        }
        return insets;
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
