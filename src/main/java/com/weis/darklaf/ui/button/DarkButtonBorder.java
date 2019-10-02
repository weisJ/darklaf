package com.weis.darklaf.ui.button;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class DarkButtonBorder implements Border, UIResource {

    public static final int BORDER_SIZE = 2;
    public static final int SHADOW_HEIGHT = 3;

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g,
                            final int x, final int y, final int width, final int height) {
        if (DarkButtonUI.isShadowVariant(c) || DarkButtonUI.isLabelButton(c)) {
            return;
        }
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(x, y);
        int arc = getArc(c);
        GraphicsContext config = new GraphicsContext(g);

        if (c.isEnabled()) {
            paintShadow(g2, width, height, arc);
        }

        g2.setColor(getBorderColor(c));
        if (DarkButtonUI.isSquare(c) && !DarkButtonUI.isForceRoundCorner(c)) {
            g2.drawRect(BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                        height - 2 * BORDER_SIZE - SHADOW_HEIGHT);
        } else {
            DarkUIUtil.paintLineBorder(g2, BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                                       height - 2 * BORDER_SIZE - SHADOW_HEIGHT, arc, true);

        }

        if (c.hasFocus()) {
            g2.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            DarkUIUtil.paintFocusBorder(g2, width, height - SHADOW_HEIGHT, arc, true);
        }
        config.restore();
    }

    protected int getArc(final Component c) {
        return DarkButtonUI.isSquare(c) && !DarkButtonUI.isForceRoundCorner(c) ? DarkButtonUI.SQUARE_ARC_SIZE
                                                                               : DarkButtonUI.ARC_SIZE;
    }

    private void paintShadow(@NotNull final Graphics2D g2, final int width, final int height, final int arc) {
        GraphicsContext context = new GraphicsContext(g2);
        Area shadowShape = new Area(new RoundRectangle2D.Double(BORDER_SIZE, BORDER_SIZE,
                                                                width - 2 * BORDER_SIZE, height - 2 * BORDER_SIZE,
                                                                arc, arc));
        Area innerArea = new Area(new RoundRectangle2D.Double(BORDER_SIZE, BORDER_SIZE,
                                                              width - 2 * BORDER_SIZE,
                                                              height - 2 * BORDER_SIZE - SHADOW_HEIGHT,
                                                              arc, arc));
        shadowShape.subtract(innerArea);
        g2.setComposite(DarkUIUtil.SHADOW_COMPOSITE);
        g2.setColor(UIManager.getColor("Button.darcula.shadow"));
        g2.fill(shadowShape);
        context.restore();
    }

    private Color getBorderColor(final Component c) {
        if (c instanceof JButton && ((JButton) c).isDefaultButton() && c.isEnabled()) {
            return UIManager.getColor("Button.darcula.defaultBorderColor");
        } else if (c.isEnabled()) {
            return UIManager.getColor("Button.darcula.activeBorderColor");
        } else {
            return UIManager.getColor("Button.darcula.inactiveBorderColor");
        }
    }

    public boolean isBorderOpaque() {
        return false;
    }

    @Contract("null -> false")
    public static boolean isThin(final Component c) {
        return c instanceof JButton
               && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.thin"));
    }

    public Insets getBorderInsets(final Component c) {
        if (DarkButtonUI.isFullShadow(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        int shadow = DarkButtonUI.isShadowVariant(c) ? 0 : SHADOW_HEIGHT;
        int pad = isThin(c) ? 4 : 8;
        if (DarkButtonUI.isSquare(c)) {
            return new InsetsUIResource(pad, pad, pad + shadow, pad);
        } else {
            return new InsetsUIResource(pad, 2 * pad, pad + shadow, 2 * pad);
        }
    }
}
