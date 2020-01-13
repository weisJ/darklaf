/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.ui.radiobutton;

import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.ui.checkbox.DarkCheckBoxUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.metal.MetalRadioButtonUI;
import java.awt.*;
import java.awt.geom.Ellipse2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkRadioButtonUI extends MetalRadioButtonUI {

    private static final int ICON_OFF = 4;
    private static final int SIZE = 13;
    private static final int BULLET_RAD = 5;
    private static final Rectangle viewRect = new Rectangle();
    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();
    private static Dimension size = new Dimension();
    private final Ellipse2D hitArea = new Ellipse2D.Float();
    protected Color background;
    protected Color inactiveBackground;
    protected Color focusBorderColor;
    protected Color focusSelectedBorderColor;
    protected Color borderColor;
    protected Color inactiveBorderColor;
    protected Color checkColor;
    protected Color inactiveCheckColor;
    protected Color focusCheckColor;
    protected Color selectedBorderColor;
    protected Color selectedBackground;
    private Icon radioIcon;
    private Icon radioDisabledIcon;
    private Icon radioFocusedIcon;
    private Icon radioSelectedIcon;
    private Icon radioSelectedDisabledIcon;
    private Icon radioSelectedFocusedIcon;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkRadioButtonUI();
    }

    @Override
    public void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, "opaque", false);
        radioIcon = UIManager.getIcon("RadioButton.unchecked.icon");
        radioDisabledIcon = UIManager.getIcon("RadioButton.uncheckedDisabled.icon");
        radioFocusedIcon = UIManager.getIcon("RadioButton.uncheckedFocused.icon");
        radioSelectedIcon = UIManager.getIcon("RadioButton.selected.icon");
        radioSelectedDisabledIcon = UIManager.getIcon("RadioButton.selectedDisabled.icon");
        radioSelectedFocusedIcon = UIManager.getIcon("RadioButton.selectedFocused.icon");
        background = UIManager.getColor("RadioButton.activeFillColor");
        inactiveBackground = UIManager.getColor("RadioButton.inactiveFillColor");
        focusBorderColor = UIManager.getColor("RadioButton.focusBorderColor");
        borderColor = UIManager.getColor("RadioButton.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("RadioButton.inactiveBorderColor");
        checkColor = UIManager.getColor("RadioButton.selectionSelectedColor");
        inactiveCheckColor = UIManager.getColor("RadioButton.selectionDisabledColor");
        focusCheckColor = UIManager.getColor("RadioButton.selectionFocusSelectedColor");
        selectedBorderColor = UIManager.getColor("RadioButton.selectedBorderColor");
        selectedBackground = UIManager.getColor("RadioButton.selectedFillColor");
        focusSelectedBorderColor = UIManager.getColor("RadioButton.focusSelectedBorderColor");
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
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        Icon icon = DarkCheckBoxUI.getIconBullet(c, g, b);
        if (icon != null) {
            icon.paintIcon(c, g, iconRect.x, iconRect.y);
        } else {
            Icon radioIcon = getRadioIcon(b);
            if (radioIcon != null) {
                radioIcon.paintIcon(c, g, iconRect.x, iconRect.y + 1);
            } else {
                paintDarkBullet(c, g, b);
            }
        }
        config.restore();
        if (text != null) {
            DarkCheckBoxUI.paintText(g, b, textRect, text, fm, getDisabledTextColor());
        }
    }

    protected String layoutRadioButton(@NotNull final AbstractButton b, final FontMetrics fm) {
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

    protected Icon getRadioIcon(@NotNull final AbstractButton b) {
        boolean selected = b.isSelected();
        boolean enabled = b.isEnabled();
        boolean hasFocus = b.hasFocus();
        return selected ? enabled ? hasFocus ? radioSelectedFocusedIcon : radioSelectedIcon
                                  : radioSelectedDisabledIcon
                        : enabled ? hasFocus ? radioFocusedIcon : radioIcon
                                  : radioDisabledIcon;
    }

    protected void paintDarkBullet(final JComponent c, final Graphics2D g, @NotNull final AbstractButton b) {
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        boolean enabled = b.isEnabled();
        g.translate(iconRect.x + ICON_OFF, iconRect.y + ICON_OFF);
        g.translate(-0.25, 0);
        paintCheckBorder(g, enabled, b.hasFocus() && b.isFocusPainted(), b.isSelected());
        if (b.isSelected()) {
            paintCheckBullet(g, enabled, b.hasFocus() && b.isFocusPainted());
        }
        g.translate(0.25, 0);
        g.translate(-iconRect.x - ICON_OFF, -iconRect.y - ICON_OFF);
        config.restore();
    }

    @Override
    public Icon getDefaultIcon() {
        return new IconUIResource(EmptyIcon.create(20));
    }

    protected void paintCheckBorder(@NotNull final Graphics2D g, final boolean enabled, final boolean focus,
                                    final boolean selected) {
        Graphics2D g2 = (Graphics2D) g.create();
        Color bgColor = getFillColor(selected, enabled);
        Color border = getBorderColor(selected, focus, enabled);
        g.setColor(bgColor);
        g.fillOval(0, 0, SIZE, SIZE);

        if (focus) {
            g2.translate(-0.2, -0.2);
            DarkUIUtil.paintFocusOval(g2, 1, 1, SIZE - 1, SIZE - 1);
        }

        g.setColor(border);
        g.drawOval(0, 0, SIZE, SIZE);

        g2.dispose();
    }

    protected void paintCheckBullet(@NotNull final Graphics2D g, final boolean enabled, final boolean focus) {
        Color color = getCheckColor(focus, enabled);
        g.setColor(color);
        g.translate(0.2, 0.2);
        g.fillOval((SIZE - BULLET_RAD) / 2, (SIZE - BULLET_RAD) / 2, BULLET_RAD, BULLET_RAD);
        g.translate(-0.2, -0.2);
    }

    protected Color getFillColor(final boolean selected, final boolean enabled) {
        return enabled ? selected ? selectedBackground : background
                       : inactiveBorderColor;
    }

    protected Color getBorderColor(final boolean selected, final boolean focus, final boolean enabled) {
        return enabled ? focus ? selected ? focusSelectedBorderColor : focusBorderColor
                               : selected ? selectedBorderColor : borderColor
                       : inactiveBorderColor;
    }

    protected Color getCheckColor(final boolean focus, final boolean enabled) {
        return enabled ? focus ? focusCheckColor : checkColor
                       : inactiveCheckColor;
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (hitArea.isEmpty() && c instanceof JRadioButton) {
            layoutRadioButton((JRadioButton) c, c.getFontMetrics(c.getFont()));
        }
        return hitArea.contains(x, y);
    }
}
