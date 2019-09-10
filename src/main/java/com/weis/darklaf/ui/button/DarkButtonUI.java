package com.weis.darklaf.ui.button;

import com.bulenkov.darcula.ui.DarculaButtonUI;
import com.bulenkov.iconloader.util.SystemInfo;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonUI;
import java.awt.*;

/**
 * Custom adaption of {@link DarculaButtonUI}.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkButtonUI extends BasicButtonUI {

    public static final int SQUARE_ARC_SIZE = 3;
    public static final int ARC_SIZE = 5;

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    @Override
    public void paint(final Graphics g, @NotNull final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        GraphicsContext config = new GraphicsContext(g2);
        int borderSize = DarkButtonBorder.BORDER_SIZE;
        if (shouldDrawBackground(c)) {
            int arc = getArcSize(c);
            if (isShadowVariant(c)) {
                var b = (AbstractButton) c;
                if (b.isEnabled() && b.getModel().isRollover()) {
                    GraphicsUtil.setupAAPainting(g2);
                    g.setColor(getShadowColor(b));
                    g.fillRoundRect(borderSize, borderSize, c.getWidth() - 2 * borderSize,
                                    c.getHeight() - 2 * borderSize, arc, arc);
                }
            } else {
                g2.setColor(getBackgroundColor(c));
                if (isSquare(c) && !isForceRoundCorner(c)) {
                    g2.fillRect(borderSize, borderSize, c.getWidth() - 2 * borderSize,
                                c.getHeight() - 2 * borderSize - DarkButtonBorder.SHADOW_HEIGHT);
                } else {
                    g2.fillRoundRect(borderSize, borderSize, c.getWidth() - 2 * borderSize,
                                     c.getHeight() - 2 * borderSize - DarkButtonBorder.SHADOW_HEIGHT, arc, arc);
                }
            }
        }
        config.restore();
        super.paint(g2, c);
    }

    protected int getArcSize(final JComponent c) {
        return isSquare(c) ? SQUARE_ARC_SIZE : ARC_SIZE;
    }

    protected Color getShadowColor(@NotNull final AbstractButton c) {
        return c.getModel().isArmed() ? UIManager.getColor("Button.shadow.click")
                                      : UIManager.getColor("Button.shadow.hover");
    }

    protected Color getBackgroundColor(@NotNull final JComponent c) {
        var defaultButton = (c instanceof JButton && (((JButton) c).isDefaultButton()));
        var rollOver = (c instanceof JButton && (((JButton) c).isRolloverEnabled()
                                                 && (((JButton) c).getModel().isRollover())));
        var clicked = rollOver && ((JButton) c).getModel().isArmed();
        if (c.isEnabled()) {
            if (defaultButton) {
                if (clicked) {
                    return UIManager.getColor("Button.darcula.defaultFillColorClick");
                } else if (rollOver) {
                    return UIManager.getColor("Button.darcula.defaultFillColorRollOver");
                } else {
                    return UIManager.getColor("Button.darcula.defaultFillColor");
                }
            } else {
                if (clicked) {
                    return UIManager.getColor("Button.darcula.activeFillColorClick");
                } else if (rollOver) {
                    return UIManager.getColor("Button.darcula.activeFillColorRollOver");
                } else {
                    return UIManager.getColor("Button.darcula.activeFillColor");
                }
            }
        } else {
            return UIManager.getColor("Button.darcula.inactiveFillColor");
        }
    }

    @Override
    protected void paintText(@NotNull final Graphics g, final JComponent c,
                             final Rectangle textRect, final String text) {
        AbstractButton button = (AbstractButton) c;
        ButtonModel model = button.getModel();
        g.setColor(getForeground(button));
        FontMetrics metrics = SwingUtilities2.getFontMetrics(c, g);
        int mnemonicIndex = button.getDisplayedMnemonicIndex();
        if (model.isEnabled()) {
            SwingUtilities2.drawStringUnderlineCharAt(c, g, text, mnemonicIndex,
                                                      textRect.x + this.getTextShiftOffset(),
                                                      textRect.y + metrics.getAscent() + getTextShiftOffset());
        } else {
            g.setColor(UIManager.getColor("Button.disabledText"));
            SwingUtilities2.drawStringUnderlineCharAt(c, g, text, -1,
                                                      textRect.x + getTextShiftOffset(),
                                                      textRect.y + metrics.getAscent() + getTextShiftOffset());
        }
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        super.update(g, c);
        if (c instanceof JButton && ((JButton) c).isDefaultButton() && !SystemInfo.isMac && !c.getFont().isBold()) {
            c.setFont(c.getFont().deriveFont(Font.BOLD));
        }
    }

    private boolean shouldDrawBackground(@NotNull final JComponent c) {
        AbstractButton button = (AbstractButton) c;
        Border border = c.getBorder();
        return c.isEnabled() && border != null && button.isContentAreaFilled() && !(c instanceof JToggleButton);
    }

    @Contract("null -> false")
    public static boolean isSquare(final Component c) {
        return c instanceof JButton && "square".equals(((JButton) c).getClientProperty("JButton.buttonType"));
    }

    @Contract("null -> false")
    public static boolean isShadowVariant(final Component c) {
        return c instanceof JButton
               && "shadow".equals(((JButton) c).getClientProperty("JButton.variant"));
    }

    @Contract("null -> false")
    public static boolean isForceRoundCorner(final Component c) {
        return c instanceof JButton
               && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.forceRoundCorner"));
    }

    protected Color getForeground(@NotNull final AbstractButton button) {
        Color fg = button.getForeground();
        if (fg instanceof UIResource && button instanceof JButton && ((JButton) button).isDefaultButton()) {
            Color selectedFg = UIManager.getColor("Button.darcula.selectedButtonForeground");
            if (selectedFg != null) {
                fg = selectedFg;
            }
        }
        return fg;
    }
}
