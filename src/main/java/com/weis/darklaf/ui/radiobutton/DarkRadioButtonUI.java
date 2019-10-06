package com.weis.darklaf.ui.radiobutton;

import com.weis.darklaf.icons.EmptyIcon;
import com.weis.darklaf.ui.checkbox.DarkCheckBoxUI;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.metal.MetalRadioButtonUI;
import java.awt.*;
import java.awt.geom.Ellipse2D;

public class DarkRadioButtonUI extends MetalRadioButtonUI {

    private static final int ICON_OFF = 4;
    private static final int SIZE = 13;
    private static final int BULLET_RAD = 5;
    private static final Rectangle viewRect = new Rectangle();
    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();
    private static Dimension size = new Dimension();
    private final Ellipse2D hitArea = new Ellipse2D.Float();


    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkRadioButtonUI();
    }

    @Override
    public synchronized void paint(final Graphics g2d, @NotNull final JComponent c) {
        Graphics2D g = (Graphics2D) g2d;
        AbstractButton b = (AbstractButton) c;

        Font f = c.getFont();
        g.setFont(f);
        FontMetrics fm = SwingUtilities2.getFontMetrics(c, g, f);

        String text = layoutRadioButton(b, fm);

        paintBackground(c, g);
        var config = GraphicsUtil.setupStrokePainting(g);
        Icon icon = DarkCheckBoxUI.getIconBullet(c, g, b);
        if (icon != null) {
            icon.paintIcon(c, g, iconRect.x, iconRect.y);
        } else {
            paintDarkBullet(c, g, b);
        }
        config.restore();
        if (text != null) {
            DarkCheckBoxUI.paintText(g, b, textRect, text, fm, getDisabledTextColor());
        }
    }

    protected String layoutRadioButton(final AbstractButton b, final FontMetrics fm) {
        Insets i = b.getInsets();
        size = b.getSize(size);
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = size.width - (i.right + viewRect.x);
        viewRect.height = size.height - (i.bottom + viewRect.y);
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        textRect.x = textRect.y = textRect.width = textRect.height = 0;

        String text = SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), getDefaultIcon(),
                                                         b.getVerticalAlignment(), b.getHorizontalAlignment(),
                                                         b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                                                         viewRect, iconRect, textRect, b.getIconTextGap());

        hitArea.setFrame(Math.max(iconRect.x, 0) + ICON_OFF,
                         Math.max(iconRect.y, 0) + ICON_OFF,
                         SIZE, SIZE);
        return text;
    }

    private void paintBackground(@NotNull final JComponent c, final Graphics2D g) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
    }

    protected void paintDarkBullet(final JComponent c, final Graphics2D g, @NotNull final AbstractButton b) {
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        boolean enabled = b.isEnabled();
        g.translate(iconRect.x + ICON_OFF, iconRect.y + ICON_OFF);
        g.translate(-0.25, 0);
        paintCheckBorder(g, enabled, b.hasFocus() && b.isFocusPainted());
        if (b.isSelected()) {
            paintCheckBullet(g, enabled);
        }
        g.translate(0.25, 0);
        g.translate(-iconRect.x - ICON_OFF, -iconRect.y - ICON_OFF);
        config.restore();
    }

    @Override
    public Icon getDefaultIcon() {
        return new IconUIResource(EmptyIcon.create(20));
    }

    static void paintCheckBorder(@NotNull final Graphics2D g, final boolean enabled, final boolean focus) {
        var g2 = (Graphics2D) g.create();
        Color bgColor = enabled ? UIManager.getColor("RadioButton.darcula.activeFillColor")
                                : UIManager.getColor("RadioButton.darcula.inactiveFillColor");
        Color borderColor = enabled ? UIManager.getColor("RadioButton.darcula.activeBorderColor")
                                    : UIManager.getColor("RadioButton.darcula.inactiveBorderColor");
        g.setColor(bgColor);
        g.fillOval(0, 0, SIZE, SIZE);
        g.setColor(borderColor);
        g.drawOval(0, 0, SIZE, SIZE);

        if (focus) {
            g2.translate(-0.2, -0.2);
            g2.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            DarkUIUtil.paintFocusOval(g2, 1, 1, SIZE - 1, SIZE - 1);
        }
        g2.dispose();
    }

    static void paintCheckBullet(@NotNull final Graphics2D g, final boolean enabled) {
        Color color = enabled ? UIManager.getColor("RadioButton.darcula.selectionEnabledColor")
                              : UIManager.getColor("RadioButton.darcula.selectionDisabledColor");
        g.setColor(color);
        g.translate(0.2, 0.2);
        g.fillOval((SIZE - BULLET_RAD) / 2, (SIZE - BULLET_RAD) / 2, BULLET_RAD, BULLET_RAD);
        g.translate(-0.2, -0.2);
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (hitArea.isEmpty() && c instanceof JRadioButton) {
            layoutRadioButton((JRadioButton) c, c.getFontMetrics(c.getFont()));
        }
        return hitArea.contains(x, y);
    }
}
