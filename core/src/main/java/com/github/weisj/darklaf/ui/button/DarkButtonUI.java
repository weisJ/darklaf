/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.button;

import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonKeyHandler;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonFocusNavigationActions;
import com.github.weisj.darklaf.util.*;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.KeyListener;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkButtonUI extends BasicButtonUI implements ButtonConstants {

    protected static final Rectangle viewRect = new Rectangle();
    protected static final Rectangle textRect = new Rectangle();
    protected static final Rectangle iconRect = new Rectangle();
    protected int borderSize;
    protected int shadowHeight;
    protected Color inactiveForeground;
    protected Color defaultForeground;
    protected Color defaultBackground;
    protected Color defaultHoverBackground;
    protected Color defaultClickBackground;
    protected Color background;
    protected Color hoverBackground;
    protected Color clickBackground;
    protected Color inactiveBackground;
    protected Color shadowHover;
    protected Color shadowClick;
    protected AbstractButton button;
    protected int arc;
    protected int squareArc;
    protected KeyListener keyListener;

    protected final AbstractButtonLayoutDelegate layoutDelegate = new AbstractButtonLayoutDelegate() {
        @Override
        public Font getFont() {
            return delegate != null ? delegate.getFont().deriveFont(Font.BOLD) : null;
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    @Override
    public String getPropertyPrefix() {
        return super.getPropertyPrefix();
    }

    @Override
    public void installUI(final JComponent c) {
        button = (AbstractButton) c;
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, PropertyKey.OPAQUE, false);
        borderSize = UIManager.getInt("Button.borderThickness");
        shadowHeight = UIManager.getInt("Button.shadowHeight");
        inactiveForeground = UIManager.getColor("Button.disabledText");
        defaultForeground = UIManager.getColor("Button.selectedButtonForeground");
        defaultBackground = UIManager.getColor("Button.defaultFillColor");
        defaultHoverBackground = UIManager.getColor("Button.defaultFillColorRollOver");
        defaultClickBackground = UIManager.getColor("Button.defaultFillColorClick");
        background = UIManager.getColor("Button.activeFillColor");
        hoverBackground = UIManager.getColor("Button.activeFillColorRollOver");
        clickBackground = UIManager.getColor("Button.activeFillColorClick");
        inactiveBackground = UIManager.getColor("Button.inactiveFillColor");
        shadowHover = UIManager.getColor("Button.borderless.hover");
        shadowClick = UIManager.getColor("Button.borderless.click");
        arc = UIManager.getInt("Button.arc");
        squareArc = UIManager.getInt("Button.squareArc");
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        keyListener = createKeyListener(b);
        b.addKeyListener(keyListener);
        ToggleButtonFocusNavigationActions.installActions(b);
    }

    protected KeyListener createKeyListener(final AbstractButton button) {
        return new DarkToggleButtonKeyHandler();
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        return new DarkButtonListener(b, this);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        b.removeKeyListener(keyListener);
        keyListener = null;
        ToggleButtonFocusNavigationActions.uninstallActions(b);
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        super.update(g, c);
        boolean isDefaultButton = ButtonConstants.isDefaultButton(c) && !SystemInfo.isMac;
        if (isDefaultButton && !c.getFont().isBold()) {
            c.setFont(c.getFont().deriveFont(Font.BOLD));
        } else if (!isDefaultButton && c.getFont().isBold()) {
            c.setFont(c.getFont().deriveFont(Font.PLAIN));
        }
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = new GraphicsContext(g);
        AbstractButton b = (AbstractButton) c;
        paintButtonBackground(g, c);

        if (ButtonConstants.isDefaultButton(b)) {
            g.setFont(g.getFont().deriveFont(Font.BOLD));
        } else if (g.getFont().isBold()) {
            g.setFont(g.getFont().deriveFont(Font.PLAIN));
        }

        String text = layout(b, c, SwingUtilities2.getFontMetrics(b, g), b.getWidth(), b.getHeight());

        paintIcon(g, b, c);
        paintText(g, b, c, text);
        config.restore();
    }

    protected void paintButtonBackground(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        if (shouldDrawBackground(c)) {
            AbstractButton b = (AbstractButton) c;
            int arc = getArc(c);
            int width = c.getWidth();
            int height = c.getHeight();
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) margin = new Insets(0, 0, 0, 0);
            if (ButtonConstants.isBorderlessVariant(c)) {
                paintShadowBackground(g, c, g2, b, arc, width, height, margin);
            } else {
                paintDefaultBackground((Graphics2D) g, b, g2, arc, width, height);
            }
        }
    }

    protected void paintDefaultBackground(final Graphics2D g, final AbstractButton c, final Graphics2D g2,
                                          final int arc, final int width, final int height) {
        int shadow = DarkButtonBorder.showDropShadow(c) ? shadowHeight : 0;
        int effectiveArc = ButtonConstants.isSquare(c) && !ButtonConstants.chooseAlternativeArc(c) ? 0 : arc;
        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);
        boolean focus = c.hasFocus() && c.isFocusPainted();

        Rectangle bgRect = getEffectiveRect(width, height, c, -(effectiveArc + 1), corner, focus);
        g2.setColor(getBackgroundColor(c));
        paintBackgroundRect(g, g2, shadow, effectiveArc, bgRect);
    }

    private void paintBackgroundRect(final Graphics2D g, final Graphics2D g2,
                                     final int shadow, final int effectiveArc, final Rectangle bgRect) {
        if (effectiveArc == 0) {
            g2.fillRect(bgRect.x, bgRect.y, bgRect.width, bgRect.height - shadow);
        } else {
            DarkUIUtil.fillRoundRect(g, bgRect.x, bgRect.y, bgRect.width, bgRect.height - shadow, effectiveArc);
        }
    }

    protected Rectangle getEffectiveRect(final int width, final int height, final AbstractButton c,
                                         final int adjustment, final AlignmentExt corner, final boolean focus) {
        Insets insetMask = new Insets(borderSize, borderSize, borderSize, borderSize);
        if (corner != null) {
            insetMask = corner.maskInsets(insetMask, adjustment);
        }
        int bx = insetMask.left;
        int by = insetMask.top;
        int bw = width - insetMask.left - insetMask.right;
        int bh = height - insetMask.top - insetMask.bottom;
        return new Rectangle(bx, by, bw, bh);
    }

    protected void paintShadowBackground(final Graphics g, final JComponent c, final Graphics2D g2,
                                         final AbstractButton b, final int arc,
                                         final int width, final int height, final Insets margin) {
        if (b.isEnabled() && b.getModel().isRollover()) {
            GraphicsUtil.setupAAPainting(g2);
            g.setColor(getShadowColor(b));
            if (ButtonConstants.isFullBorderless(c)) {
                g.fillRect(margin.left, margin.top,
                           width - margin.left - margin.right,
                           height - margin.top - margin.bottom);
            } else if (ButtonConstants.doConvertToBorderless(b)) {
                int size = Math.min(width - margin.left - margin.right,
                                    height - margin.left - margin.right);
                g.fillRoundRect((width - size) / 2, (height - size) / 2, size, size, arc, arc);
            } else {
                g.fillRoundRect(margin.left, margin.top,
                                width - margin.left - margin.right,
                                height - margin.top - margin.bottom,
                                arc, arc);
            }
        }
    }

    @Override
    protected void paintText(final Graphics g, final JComponent c,
                             final Rectangle textRect, final String text) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        AbstractButton button = (AbstractButton) c;
        ButtonModel model = button.getModel();
        g.setColor(getForeground(button));
        FontMetrics metrics = SwingUtilities2.getFontMetrics(c, g);
        int mnemonicIndex = button.getDisplayedMnemonicIndex();
        if (!model.isEnabled()) {
            mnemonicIndex = -1;
        }
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, mnemonicIndex,
                                                  textRect.x + getTextShiftOffset(),
                                                  textRect.y + metrics.getAscent() + getTextShiftOffset());
        config.restore();
    }

    protected void paintText(final Graphics g, final AbstractButton b, final JComponent c, final String text) {
        GraphicsContext context = GraphicsUtil.setupAntialiasing(g);
        g.setClip(textRect);
        if (text != null && !text.equals("")) {
            View v = (View) c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                paintText(g, b, textRect, text);
            }
        }
        context.restore();
    }

    protected void paintIcon(final Graphics g, final AbstractButton b, final JComponent c) {
        if (b.getIcon() != null) {
            g.setClip(iconRect);
            paintIcon(g, c, iconRect);
        }
    }

    protected void repaintNeighbours() {
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_LEFT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_TOP_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_RIGHT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_BOTTOM_NEIGHBOUR, button));
    }

    protected boolean shouldDrawBackground(final JComponent c) {
        if (ButtonConstants.isLabelButton(c)) return false;
        AbstractButton button = (AbstractButton) c;
        Border border = c.getBorder();
        return c.isEnabled() && border != null && button.isContentAreaFilled();
    }

    protected int getArc(final Component c) {
        if (ButtonConstants.isNoArc(c)) return 0;
        boolean square = ButtonConstants.isSquare(c);
        boolean alt = ButtonConstants.chooseAlternativeArc(c);
        return square ? alt ? arc : squareArc : alt ? squareArc : arc;
    }

    protected Color getForeground(final AbstractButton button) {
        Color fg = button.getForeground();
        if (fg instanceof UIResource
            && ButtonConstants.isDefaultButton(button)
            && !ButtonConstants.isBorderlessVariant(button)) {
            fg = defaultForeground;
        }
        if (fg instanceof UIResource && !button.getModel().isEnabled()) {
            fg = inactiveForeground;
        }
        return fg;
    }

    protected Color getBackgroundColor(final JComponent c) {
        boolean defaultButton = ButtonConstants.isDefaultButton(c);
        AbstractButton b = (AbstractButton) c;
        boolean rollOver = (b.isRolloverEnabled() || ButtonConstants.doConvertToBorderless(b))
                           && (((JButton) c).getModel().isRollover());
        boolean clicked = b.getModel().isArmed();
        if (c.isEnabled()) {
            if (defaultButton) {
                if (clicked) {
                    return defaultClickBackground;
                } else if (rollOver) {
                    return defaultHoverBackground;
                } else {
                    return defaultBackground;
                }
            } else {
                if (clicked) {
                    return clickBackground;
                } else if (rollOver) {
                    return hoverBackground;
                } else {
                    return background;
                }
            }
        } else {
            return inactiveBackground;
        }
    }

    protected Color getShadowColor(final AbstractButton c) {
        Object colorHover = c.getClientProperty(KEY_HOVER_COLOR);
        Object colorClick = c.getClientProperty(KEY_CLICK_COLOR);
        return c.getModel().isArmed() ? colorClick instanceof Color ? (Color) colorClick : shadowClick
                                      : colorHover instanceof Color ? (Color) colorHover : shadowHover;
    }

    protected String layout(final AbstractButton b, final JComponent c, final FontMetrics fm,
                            final int width, final int height) {
        Insets i = b.getInsets();
        if (!ButtonConstants.isBorderlessVariant(b)) {
            i = new Insets(i.top, borderSize, i.bottom, borderSize);
        }

        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);
        Insets insetMask = new Insets(borderSize, borderSize, borderSize, borderSize);
        if (corner != null) {
            insetMask = corner.maskInsetsInverted(insetMask, 0);
        }
        i.left -= insetMask.left;
        i.right -= insetMask.right;
        i.top -= insetMask.top;
        i.bottom -= insetMask.bottom;

        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = width - (i.right + i.left);
        viewRect.height = height - (i.bottom + i.top);

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), b.getIcon(),
                                                  b.getVerticalAlignment(), b.getHorizontalAlignment(),
                                                  b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                                                  viewRect, iconRect, textRect,
                                                  b.getText() == null || ButtonConstants.isIconOnly(b)
                                                  ? 0
                                                  : b.getIconTextGap());
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        layoutDelegate.setDelegate(b);
        return BasicGraphicsUtils.getPreferredButtonSize(layoutDelegate, b.getIconTextGap());
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (ButtonConstants.isLabelButton(c)) {
            return super.contains(c, x, y);
        }
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        int bs = borderSize;
        int arc = getArc(c);
        return new RoundRectangle2D.Float(bs, bs, c.getWidth() - 2 * bs, c.getWidth() - 2 * bs,
                                          arc, arc).contains(x, y);
    }
}
